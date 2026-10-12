library(DatabaseConnector)
library(keyring)
library(DBI)

# Project: Frailty_Komoto shareable 2022 clinical metrics
# Author: Nemo Zhou
# Date started: 2026-09-27
# Date last updated: 2026-09-27
#
# ---- Adapted for the Care Quality project (HIV vs matched non-HIV, 2016-2025) ----
# Changes from Nemo's version (everything else is unchanged):
#   1. analysis years: 2016-2025 instead of only 2022 (and the "2022 only" check is relaxed)
#   2. input table: our matched table ps_matched (patient_id, analysis_year, index_date, ...)
#   3. output table names: "cq_" instead of "share_2022_" (they are no longer 2022 only)
#   4. replace_existing = TRUE, so a rerun replaces old outputs instead of stopping
#   5. connection: same way as our training code (createConnectionDetails), so the
#      ohdsilab package is not needed; driver folder set to our drivers folder
#   6. lookup folder: Code/Clinical Metric Look Up Tables (and its CCW30 subfolder)
#
# ---- Purpose ----
# Define the configuration and reusable Redshift helpers used by the shareable
# CFI, Gagne, CCW-30, and HIV scripts in this folder. This file does not query
# Redshift or create tables. Each numbered construction script sources this file
# and opens its own connection inside a function so cleanup occurs only after
# that stage finishes.

shareable_clinical_metrics_default_config <- function() {
  project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

  list(
    analysis_year = 2016:2025,   # CHANGED: was 2022L
    komodo_schema = "komodo_202606",
    write_schema = NULL,
    eligibility_table = "ps_matched",   # CHANGED: our matched HIV + non-HIV table
    normalized_dx_table = "normalized_dx_events",
    normalized_procedure_table = "normalized_procedure_events",
    lookup_dir = file.path(   # CHANGED: shorter folder path (no "shareable_clinical_metrics")
      project_root,
      "Code",
      "Clinical Metric Look Up Tables"
    ),
    ccw30_lookup_dir = file.path(   # CHANGED: shorter folder path (no "shareable_clinical_metrics")
      project_root,
      "Code",
      "Clinical Metric Look Up Tables",
      "CCW30"
    ),
    metric_ids_table = "cq_metric_ids",
    cfi_procedure_presence_table = "cq_cfi_procedure_presence",
    dx_candidate_table = "cq_cfi_gagne_dx_candidates",
    cfi_feature_matches_table = "cq_cfi_feature_matches",
    gagne_group_matches_table = "cq_gagne_group_matches",
    cfi_scores_table = "cq_cfi_scores",
    gagne_scores_table = "cq_gagne_scores",
    ccw30_evidence_table = "cq_ccw30_matched_evidence",
    hiv_evidence_table = "cq_hiv_diagnosis_evidence",
    ccw30_conditions_long_table = "cq_ccw30_conditions_long",
    ccw30_indicators_table = "cq_ccw30_condition_indicators",
    ccw30_counts_table = "cq_ccw30_total_counts",
    hiv_status_table = "cq_hiv_status",
    final_table = "cq_clinical_metrics",   # CHANGED (all output names): "share_2022_" -> "cq_"
    hiv_non_inpatient_min_distinct_dates = 2L,
    cfi_intercept = 0.10288,
    diagnosis_candidate_prefix_length = 3L,
    replace_existing = TRUE,   # CHANGED: was FALSE (rerun replaces old outputs)
    jdbc_jar_folder = "D:/Users/wang.life/Desktop/drivers"   # CHANGED: our Redshift driver folder
  )
}

get_shareable_clinical_metrics_config <- function() {
  config <- utils::modifyList(
    shareable_clinical_metrics_default_config(),
    getOption("frailty.shareable_clinical_metrics.config", list())
  )

  # CHANGED: Nemo's version allowed only 2022. We allow any years from 2016 to 2025.
  config$analysis_year <- as.integer(config$analysis_year)
  if (length(config$analysis_year) < 1L || any(is.na(config$analysis_year)) ||
      any(config$analysis_year < 2016L | config$analysis_year > 2025L)) {
    stop("analysis_year must be years between 2016 and 2025.")
  }

  config$hiv_non_inpatient_min_distinct_dates <- as.integer(
    config$hiv_non_inpatient_min_distinct_dates
  )
  if (
    length(config$hiv_non_inpatient_min_distinct_dates) != 1L ||
      is.na(config$hiv_non_inpatient_min_distinct_dates) ||
      config$hiv_non_inpatient_min_distinct_dates < 1L
  ) {
    stop("hiv_non_inpatient_min_distinct_dates must be a positive integer.")
  }

  config$diagnosis_candidate_prefix_length <- as.integer(
    config$diagnosis_candidate_prefix_length
  )
  if (
    length(config$diagnosis_candidate_prefix_length) != 1L ||
      is.na(config$diagnosis_candidate_prefix_length) ||
      config$diagnosis_candidate_prefix_length < 1L
  ) {
    stop("diagnosis_candidate_prefix_length must be a positive integer.")
  }

  config$cfi_intercept <- as.numeric(config$cfi_intercept)
  if (length(config$cfi_intercept) != 1L || is.na(config$cfi_intercept)) {
    stop("cfi_intercept must be one numeric value.")
  }

  if (is.null(config$write_schema) || !nzchar(config$write_schema)) {
    config$write_schema <- paste0("work_", keyring::key_get("db_username"))
  }

  table_fields <- c(
    "eligibility_table", "normalized_dx_table", "normalized_procedure_table",
    "metric_ids_table", "cfi_procedure_presence_table", "dx_candidate_table",
    "cfi_feature_matches_table", "gagne_group_matches_table", "cfi_scores_table",
    "gagne_scores_table", "ccw30_evidence_table", "hiv_evidence_table",
    "ccw30_conditions_long_table", "ccw30_indicators_table", "ccw30_counts_table",
    "hiv_status_table", "final_table"
  )
  table_names <- unlist(config[table_fields], use.names = FALSE)
  valid_table_name <- grepl("^[A-Za-z_][A-Za-z0-9_]*$", table_names)
  if (any(is.na(table_names) | !nzchar(table_names) | !valid_table_name)) {
    stop("All configured table names must be nonempty Redshift identifiers.")
  }

  output_fields <- setdiff(table_fields, c(
    "eligibility_table", "normalized_dx_table", "normalized_procedure_table"
  ))
  output_names <- unlist(config[output_fields], use.names = FALSE)
  if (anyDuplicated(output_names)) {
    stop("Every shareable workflow output table must have a unique name.")
  }

  config$replace_existing <- isTRUE(config$replace_existing)
  config
}

connect_shareable_komodo <- function(config) {
  if (nzchar(config$jdbc_jar_folder)) {
    Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = config$jdbc_jar_folder)
  }

  # CHANGED: connect the same way as our training code (no ohdsilab package needed)
  con <- DatabaseConnector::connect(DatabaseConnector::createConnectionDetails(
    dbms = "redshift",
    server = "ohdsi-lab-redshift-cluster-prod.clsyktjhufn7.us-east-1.redshift.amazonaws.com/ohdsi_lab",
    port = 5439,
    user = keyring::key_get("db_username"),
    password = keyring::key_get("db_password")
  ))
  options(con.default.value = con)
  options(schema.default.value = config$komodo_schema)
  options(write_schema.default.value = config$write_schema)
  con
}

disconnect_shareable_komodo <- function(con) {
  if (!is.null(con)) {
    try(DatabaseConnector::disconnect(con), silent = TRUE)
  }
  invisible(NULL)
}

quote_identifier <- function(identifier) {
  paste0('"', gsub('"', '""', identifier, fixed = TRUE), '"')
}

qualified_identifier <- function(schema, table) {
  paste(quote_identifier(schema), quote_identifier(table), sep = ".")
}

sql_string <- function(value) {
  paste0("'", gsub("'", "''", value, fixed = TRUE), "'")
}

read_lookup_csv <- function(path) {
  if (!file.exists(path)) {
    stop("Required lookup file was not found: ", path)
  }
  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    colClasses = "character",
    check.names = FALSE,
    na.strings = c("", "NA")
  )
}

require_columns <- function(data, columns, label) {
  missing_columns <- setdiff(columns, names(data))
  if (length(missing_columns) > 0L) {
    stop(label, " is missing columns: ", paste(missing_columns, collapse = ", "))
  }
  invisible(data)
}

table_exists <- function(con, schema, table) {
  result <- tryCatch(
    DBI::dbGetQuery(
      con,
      paste0(
        "SELECT COUNT(*)::INTEGER AS n\n",
        "FROM information_schema.tables\n",
        "WHERE table_schema = ", sql_string(schema), "\n",
        "  AND table_name = ", sql_string(table)
      )
    ),
    error = function(e) NULL
  )
  if (!is.null(result) && result$n[[1]] > 0L) {
    return(TRUE)
  }

  # Some readable Redshift external tables are not exposed consistently through
  # information_schema. A zero-row query is the authoritative fallback.
  isTRUE(tryCatch({
    DBI::dbGetQuery(
      con,
      paste0("SELECT * FROM ", qualified_identifier(schema, table), " LIMIT 0")
    )
    TRUE
  }, error = function(e) FALSE))
}

get_table_columns <- function(con, schema, table) {
  result <- DBI::dbGetQuery(
    con,
    paste0("SELECT * FROM ", qualified_identifier(schema, table), " LIMIT 0")
  )
  tolower(names(result))
}

require_table_columns <- function(con, schema, table, columns) {
  if (!table_exists(con, schema, table)) {
    stop("Required table was not found: ", schema, ".", table)
  }
  missing_columns <- setdiff(tolower(columns), get_table_columns(con, schema, table))
  if (length(missing_columns) > 0L) {
    stop(
      schema,
      ".",
      table,
      " is missing columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  invisible(TRUE)
}

prepare_output_table <- function(con, config, table) {
  if (!table_exists(con, config$write_schema, table)) {
    return(invisible(FALSE))
  }
  if (!config$replace_existing) {
    stop(
      "Output table already exists: ",
      config$write_schema,
      ".",
      table,
      ". Set replace_existing = TRUE only when an intentional rebuild is required."
    )
  }
  DatabaseConnector::executeSql(
    con,
    paste0("DROP TABLE ", qualified_identifier(config$write_schema, table), ";"),
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
  invisible(TRUE)
}

execute_sql <- function(con, sql) {
  DatabaseConnector::executeSql(
    con,
    sql,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
  invisible(NULL)
}

print_aggregate_query <- function(con, label, sql) {
  message(label)
  result <- DBI::dbGetQuery(con, sql)
  print(result)
  invisible(result)
}

insert_data_frame <- function(
  con,
  table_identifier,
  columns,
  data,
  numeric_columns = character(),
  chunk_size = 1000L
) {
  if (nrow(data) == 0L) {
    return(invisible(NULL))
  }

  for (start_row in seq.int(1L, nrow(data), by = chunk_size)) {
    end_row <- min(start_row + chunk_size - 1L, nrow(data))
    chunk <- data[start_row:end_row, columns, drop = FALSE]
    values <- apply(chunk, 1L, function(row) {
      encoded <- vapply(names(row), function(column) {
        value <- row[[column]]
        if (is.na(value)) {
          "NULL"
        } else if (column %in% numeric_columns) {
          as.character(value)
        } else {
          sql_string(value)
        }
      }, character(1))
      paste0("(", paste(encoded, collapse = ", "), ")")
    })

    execute_sql(
      con,
      paste0(
        "INSERT INTO ", table_identifier,
        " (", paste(quote_identifier(columns), collapse = ", "), ") VALUES ",
        paste(values, collapse = ", "), ";"
      )
    )
  }
  invisible(NULL)
}

start_phase <- function(label) {
  started_at <- Sys.time()
  message(format(started_at, "[%Y-%m-%d %H:%M:%S] START "), label)
  flush.console()
  started_at
}

finish_phase <- function(label, started_at) {
  elapsed_minutes <- as.numeric(difftime(Sys.time(), started_at, units = "mins"))
  message(
    format(Sys.time(), "[%Y-%m-%d %H:%M:%S] DONE "),
    label,
    sprintf(" (%.1f minutes)", elapsed_minutes)
  )
  flush.console()
  invisible(elapsed_minutes)
}

check_one_row_per_patient_year <- function(con, table_identifier, label) {
  qa <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*)::BIGINT AS n_rows,\n",
      "       COUNT(DISTINCT patid || '|' || analysis_year::VARCHAR)::BIGINT AS n_keys\n",
      "FROM ", table_identifier
    )
  )
  if (qa$n_rows[[1]] == 0 || qa$n_rows[[1]] != qa$n_keys[[1]]) {
    stop(label, " does not contain exactly one row per patient-year.")
  }
  invisible(qa)
}
