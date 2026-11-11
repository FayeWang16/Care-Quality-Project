# Project: Komodo_generalizability - shareable index-based polypharmacy
# Author: Nemo Zhou
# Date started: 2026-11-09
# Date last updated: 2026-11-09
# Purpose: Configure the independent 12-month pre-index pharmacy workflow and
# provide connection, materialization, file, and interval-SQL helpers. Sourcing
# this file defines functions only; credentials are read when a stage runs.

polypharmacy_defaults <- function() {
  list(
    study_root = getwd(),
    komodo_schema = getOption("komodo.schema", Sys.getenv("KOMODO_SCHEMA", "komodo_202610")),
    write_schema = NULL,
    cohort_schema = NULL, cohort_table = NULL,
    index_id_column = "index_id", patient_id_column = "patient_id",
    index_date_column = "index_date",
    table_prefix = "share_poly12m_",
    lookback_months = 12L, minimum_classes = 5L, minimum_days = 90L,
    mapping_version = NULL,
    relation_policy = "atcprod_only",
    replace_existing = FALSE,
    jdbc_jar_folder = Sys.getenv("DATABASECONNECTOR_JAR_FOLDER", "")
  )
}

polypharmacy_config <- function() {
  cfg <- utils::modifyList(polypharmacy_defaults(),
                          getOption("komodo.shareable_polypharmacy.config", list()))
  required <- c("cohort_table", "mapping_version")
  for (field in required) {
    if (length(cfg[[field]]) != 1L || is.na(cfg[[field]]) || !nzchar(cfg[[field]])) {
      stop("Set ", field, " in options(komodo.shareable_polypharmacy.config = list(...)).")
    }
  }
  for (field in c("lookback_months", "minimum_classes", "minimum_days")) {
    value <- cfg[[field]]
    if (length(value) != 1L || is.na(value) || value < 1 || value != as.integer(value)) {
      stop(field, " must be a positive integer.")
    }
    cfg[[field]] <- as.integer(value)
  }
  if (is.null(cfg$write_schema)) cfg$write_schema <- paste0("work_", keyring::key_get("db_username"))
  if (is.null(cfg$cohort_schema)) cfg$cohort_schema <- cfg$write_schema
  for (field in c("komodo_schema", "write_schema", "cohort_schema", "cohort_table",
                  "index_id_column", "patient_id_column", "index_date_column", "table_prefix")) {
    value <- cfg[[field]]
    if (length(value) != 1L || is.na(value) || !grepl("^[A-Za-z0-9_]+$", value)) {
      stop("Invalid SQL identifier in ", field, ".")
    }
  }
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9_-]{0,127}$", cfg$mapping_version)) stop("Use a simple mapping_version label of at most 128 characters.")
  if (length(cfg$relation_policy) != 1L || !cfg$relation_policy %in% c("atcprod_only", "n2c_combined")) {
    stop("relation_policy must be atcprod_only or n2c_combined.")
  }
  cfg$study_root <- normalizePath(cfg$study_root, winslash = "/", mustWork = TRUE)
  cfg$output_dir <- file.path(cfg$study_root, "Outputs", "4_shareable_polypharmacy")
  cfg$mapping_dir <- file.path(cfg$output_dir, "mapping", cfg$mapping_version)
  cfg$ndc_path <- file.path(cfg$output_dir, "unique_ndc11.txt")
  cfg$crosswalk_path <- file.path(cfg$mapping_dir, "ndc11_atc4.csv")
  cfg$manifest_path <- file.path(cfg$mapping_dir, "manifest.json")
  cfg$tables <- stats::setNames(paste0(cfg$table_prefix,
    c("index_windows", "fills", "crosswalk", "intervals", "metrics", "pipeline_state")),
    c("indices", "fills", "crosswalk", "intervals", "metrics", "state"))
  if (any(nchar(cfg$tables) > 127L)) stop("Table prefix is too long.")
  if (cfg$cohort_schema == cfg$write_schema && cfg$cohort_table %in% cfg$tables) {
    stop("Input cohort must not be an output table.")
  }
  cfg
}

pp_connect <- function(cfg) {
  for (pkg in c("ohdsilab", "DatabaseConnector", "DBI", "keyring", "dplyr", "dbplyr", "jsonlite")) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop("Required package unavailable: ", pkg, ". Use renv::install().")
  }
  if (nzchar(cfg$jdbc_jar_folder)) Sys.setenv(DATABASECONNECTOR_JAR_FOLDER = cfg$jdbc_jar_folder)
  ohdsilab::ohdsilab_connect(username = keyring::key_get("db_username"),
                           password = keyring::key_get("db_password"))
}

pp_ident <- function(x) paste0('"', gsub('"', '""', x, fixed = TRUE), '"')
pp_table <- function(schema, table) paste(pp_ident(schema), pp_ident(table), sep = ".")
pp_literal <- function(x) paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
pp_render <- function(sql, ...) {
  values <- list(...)
  for (key in names(values)) sql <- gsub(paste0("{", key, "}"), values[[key]], sql, fixed = TRUE)
  sql
}
pp_query <- function(con, sql) {
  result <- DBI::dbGetQuery(con, sql)
  names(result) <- tolower(names(result))
  result
}
pp_exec <- function(con, sql) DatabaseConnector::executeSql(
  con, sql, progressBar = FALSE, reportOverallTime = FALSE)
pp_phase <- function(con, label, sql) {
  started <- Sys.time()
  message(format(started, "[%Y-%m-%d %H:%M:%S] START "), label)
  pp_exec(con, sql)
  message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] DONE "), label,
          sprintf(" (%.1f minutes)", as.numeric(difftime(Sys.time(), started, units = "mins"))))
}
pp_require_columns <- function(con, schema, table, columns) {
  actual <- names(pp_query(con, paste0("SELECT * FROM ", pp_table(schema, table), " LIMIT 0")))
  missing <- setdiff(tolower(columns), actual)
  if (length(missing)) stop(schema, ".", table, " is missing: ", paste(missing, collapse = ", "))
}
pp_check_outputs <- function(con, cfg, keys) {
  # Check all destinations before a stage writes anything. Final results are
  # replaced only when the caller explicitly requests a rebuild.
  for (key in keys) {
    exists <- tryCatch({
      pp_query(con, paste0("SELECT * FROM ", pp_table(cfg$write_schema, cfg$tables[[key]]), " LIMIT 0"))
      TRUE
    }, error = function(e) FALSE)
    if (exists && !isTRUE(cfg$replace_existing)) stop("Output exists: ", cfg$tables[[key]],
      ". To rebuild intentionally, set replace_existing = TRUE.")
  }
}

pp_new_id <- function() {
  paste0(format(Sys.time(), "%Y%m%dT%H%M%OS6"), "_", Sys.getpid(), "_", basename(tempfile()))
}

pp_begin_extraction <- function(con, cfg, extraction_id) {
  # Mark downstream outputs stale BEFORE replacing any input table. Even a
  # failed extraction must not leave earlier intervals eligible for scoring.
  state <- pp_table(cfg$write_schema, cfg$tables[["state"]])
  fields <- c("extraction_id", "lookback_months", "komodo_schema", "cohort_schema", "cohort_table",
    "index_id_column", "patient_id_column", "index_date_column", "input_status", "exposure_status", "metrics_status")
  values <- c(extraction_id, cfg$lookback_months, cfg$komodo_schema, cfg$cohort_schema, cfg$cohort_table,
    cfg$index_id_column, cfg$patient_id_column, cfg$index_date_column, "BUILDING", "STALE", "STALE")
  pp_phase(con, "Start extraction and invalidate downstream status", paste0(
    "CREATE TABLE IF NOT EXISTS ", state, " (
      extraction_id VARCHAR(128), lookback_months INTEGER,
      komodo_schema VARCHAR(128), cohort_schema VARCHAR(128), cohort_table VARCHAR(128),
      index_id_column VARCHAR(128), patient_id_column VARCHAR(128), index_date_column VARCHAR(128),
      input_status VARCHAR(16), exposure_id VARCHAR(128), exposure_status VARCHAR(16),
      mapping_version VARCHAR(128), relation_policy VARCHAR(32), metrics_status VARCHAR(16)
    ); DELETE FROM ", state, "; INSERT INTO ", state, " (", paste(fields, collapse = ","),
    ") VALUES (", paste(pp_literal(values), collapse = ","), ");"))
}

pp_state <- function(con, cfg, exposures = FALSE) {
  state <- tryCatch(pp_query(con, paste0("SELECT * FROM ", pp_table(cfg$write_schema, cfg$tables[["state"]]))),
    error = function(e) stop("Saved extraction metadata is unavailable. Run the updated stage 2 first."))
  pp_validate_state(state, cfg, exposures)
  state
}

pp_validate_state <- function(state, cfg, exposures = FALSE) {
  if (nrow(state) != 1L || !identical(state$input_status, "READY")) {
    stop("Input extraction is incomplete. Run stage 2 successfully before continuing.")
  }
  for (field in c("lookback_months", "komodo_schema", "cohort_schema", "cohort_table",
                  "index_id_column", "patient_id_column", "index_date_column")) {
    if (!identical(as.character(state[[field]]), as.character(cfg[[field]]))) {
      stop("Configuration differs from the saved extraction: ", field, ". Rerun stage 2.")
    }
  }
  if (exposures && (!identical(state$exposure_status, "READY") ||
      !identical(state$mapping_version, cfg$mapping_version) ||
      !identical(state$relation_policy, cfg$relation_policy) ||
      length(state$exposure_id) != 1L || is.na(state$exposure_id) || !nzchar(state$exposure_id))) {
    stop("Exposure build is incomplete or uses another mapping policy/version. Rerun stage 3.")
  }
  invisible(TRUE)
}

pp_update_state <- function(con, cfg, extraction_id, ...) {
  values <- list(...)
  assignments <- paste0(pp_ident(names(values)), " = ", pp_literal(unlist(values, use.names = FALSE)))
  pp_exec(con, paste0("UPDATE ", pp_table(cfg$write_schema, cfg$tables[["state"]]), " SET ",
    paste(assignments, collapse = ","), " WHERE extraction_id = ", pp_literal(extraction_id), ";"))
}

pp_lineage_sql <- function(table, extraction_id, exposure_id = NULL, mapping_version = NULL, relation_policy = NULL) {
  values <- list(extraction_id = extraction_id, exposure_id = exposure_id,
                 mapping_version = mapping_version, relation_policy = relation_policy)
  values <- values[!vapply(values, is.null, logical(1))]
  invalid <- vapply(names(values), function(field) paste0(field, " IS NULL OR ", field,
    " <> ", pp_literal(values[[field]])), character(1))
  paste0("SELECT COUNT(*) AS n FROM ", table, " WHERE ", paste(invalid, collapse = " OR "))
}

pp_build <- function(con, cfg, key, select_sql, sort_column = "index_id") {
  target <- pp_table(cfg$write_schema, cfg$tables[[key]])
  # The SELECT is already restricted to the supplied full cohort.
  pp_phase(con, paste("Build", cfg$tables[[key]]), paste0(
    "DROP TABLE IF EXISTS ", target, ";\nCREATE TABLE ", target,
    " DISTSTYLE KEY DISTKEY(", pp_ident(sort_column), ") SORTKEY(", pp_ident(sort_column), ") AS\n", select_sql, ";"))
}
pp_archive <- function(path, cfg) {
  if (!file.exists(path)) return(invisible(NULL))
  stamp <- paste0(format(Sys.time(), "%Y%m%dT%H%M%S"), "_", Sys.getpid(), "_", basename(tempfile()))
  archive <- file.path(cfg$study_root, "Outputs", "archive", "shareable_polypharmacy", stamp)
  dir.create(archive, recursive = TRUE, showWarnings = FALSE)
  if (!file.rename(path, file.path(archive, basename(path)))) stop("Could not archive ", path)
}
pp_write_csv <- function(data, name, cfg) {
  dir.create(cfg$output_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(cfg$output_dir, name)
  pp_archive(path, cfg)
  utils::write.csv(data, path, row.names = FALSE, na = "")
}
pp_insert <- function(con, target, data) {
  if (!nrow(data)) return(invisible(NULL))
  started <- Sys.time()
  message(format(started, "[%Y-%m-%d %H:%M:%S] START "), "Load drug-class crosswalk")
  for (start in seq.int(1L, nrow(data), by = 500L)) {
    part <- data[start:min(start + 499L, nrow(data)), , drop = FALSE]
    rows <- apply(part, 1L, function(row) paste0("(", paste(
      ifelse(is.na(row), "NULL", pp_literal(row)), collapse = ","), ")"))
    pp_exec(con, paste0("INSERT INTO ", target, " (", paste(pp_ident(names(data)), collapse = ","),
                        ") VALUES ", paste(rows, collapse = ","), ";"))
  }
  message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] DONE "), "Load drug-class crosswalk",
          sprintf(" (%.1f minutes)", as.numeric(difftime(Sys.time(), started, units = "mins"))))
  invisible(NULL)
}

pp_exposure_sql <- function(indices, fills, crosswalk) {
  pp_render("SELECT DISTINCT f.index_id, c.atc3,
    GREATEST(f.fill_date, i.window_start) AS exposure_start,
    CAST(LEAST(DATEADD(day, f.days_supply - 1, f.fill_date), i.window_end) AS DATE) AS exposure_end
    FROM {fills} f INNER JOIN {indices} i ON f.index_id = i.index_id AND f.patient_id = i.patient_id
    INNER JOIN {crosswalk} c ON f.ndc11 = c.ndc11 AND c.mapping_status = 'mapped'
    WHERE f.fill_date <= i.window_end
      AND DATEADD(day, f.days_supply - 1, f.fill_date) >= i.window_start",
    indices = indices, fills = fills, crosswalk = crosswalk)
}

pp_merge_sql <- function(exposures) {
  # Running MAX, rather than LAG(end), correctly handles nested intervals.
  pp_render("WITH ordered AS (
    SELECT *, MAX(exposure_end) OVER (PARTITION BY index_id, atc3
      ORDER BY exposure_start, exposure_end
      ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS prior_end
    FROM {exposures}
  ), marked AS (
    SELECT *, CASE WHEN prior_end IS NULL OR exposure_start > DATEADD(day, 1, prior_end)
      THEN 1 ELSE 0 END AS new_group FROM ordered
  ), grouped AS (
    SELECT *, SUM(new_group) OVER (PARTITION BY index_id, atc3
      ORDER BY exposure_start, exposure_end ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS grp
    FROM marked
  ) SELECT index_id, atc3, MIN(exposure_start) AS interval_start, MAX(exposure_end) AS interval_end
    FROM grouped GROUP BY index_id, atc3, grp", exposures = exposures)
}

pp_spans_sql <- function(intervals) {
  pp_render("WITH changes AS (
    SELECT index_id, interval_start AS event_date, 1 AS delta FROM {intervals}
    UNION ALL
    SELECT index_id, DATEADD(day, 1, interval_end) AS event_date, -1 AS delta FROM {intervals}
  ), daily_changes AS (
    SELECT index_id, event_date, SUM(delta) AS delta FROM changes GROUP BY index_id, event_date
  ), running AS (
    SELECT index_id, event_date,
      SUM(delta) OVER (PARTITION BY index_id ORDER BY event_date
        ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS active_classes,
      LEAD(event_date) OVER (PARTITION BY index_id ORDER BY event_date) AS next_date
    FROM daily_changes
  ) SELECT index_id, event_date, next_date, active_classes,
      DATEDIFF(day, event_date, next_date) AS span_days
    FROM running WHERE next_date > event_date", intervals = intervals)
}
