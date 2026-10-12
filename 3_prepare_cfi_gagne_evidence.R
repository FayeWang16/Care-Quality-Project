source("Code/shareable_clinical_metrics/1_configure_clinical_metrics.R")

# Project: Frailty_Komoto shareable clinical metrics (2016-2025)
# Author: Lifei Wang
# Acknowledgements: Nemo Zhou
# Date started: 2026-10-12
# Date last updated: 2026-10-12
#
#
# ---- Adapted for the Care Quality project ----
# CHANGED: works for every year in config$analysis_year (2016-2025), not only 2022:
#   - keeps the years in config$analysis_year (was only 2022)
#   - each row only uses records from its own year: analysis_start_date to
#     analysis_end_date from File 2 (was always 2022-01-01 to 2022-12-31)
#   - an extra fixed date range (first Jan 1 to last Dec 31) is kept so Redshift
#     can still skip records outside 2016-2025 quickly
#
# ---- Purpose ----
# Build compact CFI and Gagne feature evidence from the normalized Komodo code
# tables. Diagnosis events are scanned once for both metrics and reduced early
# to distinct patient-year/code candidates. CFI-relevant procedures are scanned
# separately and reduced to distinct patient-year/code presence. Exact, prefix,
# and range rules are then applied to distinct codes rather than repeatedly to
# the full normalized event tables.

candidate_prefixes_for_range <- function(start_value, end_value, max_prefix_length) {
  if (is.na(start_value) || is.na(end_value)) {
    return(character())
  }
  start_chars <- strsplit(start_value, "", fixed = TRUE)[[1]]
  end_chars <- strsplit(end_value, "", fixed = TRUE)[[1]]
  shared_length <- min(length(start_chars), length(end_chars))
  if (shared_length == 0L) {
    return(character())
  }

  equal_positions <- start_chars[seq_len(shared_length)] == end_chars[seq_len(shared_length)]
  first_mismatch <- which(!equal_positions)
  prefix_length <- if (length(first_mismatch) == 0L) {
    shared_length
  } else {
    max(1L, first_mismatch[[1]] - 1L)
  }

  if (prefix_length == 1L && start_chars[[1]] != end_chars[[1]]) {
    ordered_leads <- c(LETTERS, as.character(0:9))
    start_index <- match(start_chars[[1]], ordered_leads)
    end_index <- match(end_chars[[1]], ordered_leads)
    if (is.na(start_index) || is.na(end_index) || start_index > end_index) {
      stop("Cannot derive candidate prefixes for range ", start_value, "-", end_value, ".")
    }
    return(ordered_leads[start_index:end_index])
  }

  substr(start_value, 1L, min(max_prefix_length, prefix_length))
}

make_candidate_prefixes <- function(lookup, max_prefix_length) {
  prefix_list <- lapply(seq_len(nrow(lookup)), function(row_number) {
    row <- lookup[row_number, , drop = FALSE]
    values <- if (row$match_type %in% c("exact", "prefix")) {
      row$match_value
    } else if (row$match_type == "range") {
      candidate_prefixes_for_range(
        row$range_start,
        row$range_end,
        max_prefix_length
      )
    } else {
      character()
    }
    values <- values[!is.na(values) & nzchar(values)]
    unique(substr(values, 1L, pmin(max_prefix_length, nchar(values))))
  })

  expanded <- lookup[rep(seq_len(nrow(lookup)), lengths(prefix_list)), , drop = FALSE]
  expanded$candidate_prefix <- unlist(prefix_list, use.names = FALSE)
  expanded$candidate_prefix_length <- nchar(expanded$candidate_prefix)
  expanded[!is.na(expanded$candidate_prefix) & nzchar(expanded$candidate_prefix), ]
}

run_prepare_cfi_gagne_evidence <- function() {
  config <- get_shareable_clinical_metrics_config()
  con <- connect_shareable_komodo(config)
  on.exit(disconnect_shareable_komodo(con), add = TRUE)

  require_table_columns(
    con,
    config$write_schema,
    config$metric_ids_table,
    c("patid", "patient_id", "analysis_year", "analysis_start_date", "analysis_end_date")
  )
  require_table_columns(
    con,
    config$komodo_schema,
    config$normalized_dx_table,
    c("patient_id", "event_date", "dx_code")
  )
  require_table_columns(
    con,
    config$komodo_schema,
    config$normalized_procedure_table,
    c("patient_id", "event_date", "procedure_code")
  )

  diagnosis_lookup <- read_lookup_csv(file.path(
    config$lookup_dir,
    "0.6_unified_diagnosis_rule_lookup.csv"
  ))
  require_columns(
    diagnosis_lookup,
    c(
      "lookup_version", "metric", "feature_id", "feature_name", "code_system",
      "match_value", "range_start", "range_end", "range_end_inclusive",
      "match_type", "final_match_after_flattening"
    ),
    "Unified diagnosis lookup"
  )
  diagnosis_lookup$metric <- toupper(diagnosis_lookup$metric)
  diagnosis_lookup$match_type <- tolower(diagnosis_lookup$match_type)
  diagnosis_lookup$lookup_row_id <- seq_len(nrow(diagnosis_lookup))
  diagnosis_lookup <- diagnosis_lookup[
    !is.na(diagnosis_lookup$code_system) &
      diagnosis_lookup$code_system == "ICD10CM" &
      !is.na(diagnosis_lookup$metric) &
      diagnosis_lookup$metric %in% c("CFI", "GAGNE") &
      !is.na(diagnosis_lookup$final_match_after_flattening) &
      toupper(diagnosis_lookup$final_match_after_flattening) == "TRUE",
    c(
      "lookup_row_id", "lookup_version", "metric", "feature_id", "feature_name",
      "match_value", "range_start", "range_end", "range_end_inclusive", "match_type"
    )
  ]
  if (nrow(diagnosis_lookup) == 0L) {
    stop("No active CFI or Gagne ICD-10-CM diagnosis lookup rows were found.")
  }
  if (any(!diagnosis_lookup$match_type %in% c("exact", "prefix", "range"))) {
    stop("CFI/Gagne diagnosis lookup contains an unsupported match type.")
  }
  diagnosis_lookup <- make_candidate_prefixes(
    diagnosis_lookup,
    config$diagnosis_candidate_prefix_length
  )
  if (nrow(diagnosis_lookup) == 0L) {
    stop("No CFI/Gagne diagnosis lookup row produced a candidate prefix.")
  }

  procedure_lookup <- read_lookup_csv(file.path(
    config$lookup_dir,
    "0.6_cfi_procedure_lookup.csv"
  ))
  require_columns(
    procedure_lookup,
    c(
      "lookup_version", "metric", "feature_id", "feature_name", "code_system",
      "range_start", "range_end", "match_type"
    ),
    "CFI procedure lookup"
  )
  procedure_lookup$metric <- toupper(procedure_lookup$metric)
  procedure_lookup$match_type <- tolower(procedure_lookup$match_type)
  procedure_lookup <- procedure_lookup[
    !is.na(procedure_lookup$metric) & procedure_lookup$metric == "CFI" &
      !is.na(procedure_lookup$code_system) &
      procedure_lookup$code_system %in% c("CPT_HCPCS", "CPTHCPCS") &
      !is.na(procedure_lookup$match_type) & procedure_lookup$match_type == "range" &
      !is.na(procedure_lookup$range_start) & nzchar(procedure_lookup$range_start) &
      !is.na(procedure_lookup$range_end) & nzchar(procedure_lookup$range_end),
    c(
      "lookup_version", "metric", "feature_id", "feature_name",
      "range_start", "range_end", "match_type"
    )
  ]
  if (nrow(procedure_lookup) == 0L) {
    stop("No active CFI CPT/HCPCS procedure ranges were found.")
  }

  output_tables <- c(
    config$cfi_procedure_presence_table,
    config$dx_candidate_table,
    config$cfi_feature_matches_table,
    config$gagne_group_matches_table
  )
  for (table in output_tables) {
    prepare_output_table(con, config, table)
  }

  ids_identifier <- qualified_identifier(config$write_schema, config$metric_ids_table)
  # CHANGED: years and the overall date range, e.g. 2016-01-01 to 2026-01-01
  year_list  <- paste(config$analysis_year, collapse = ", ")
  first_date <- paste0("DATE '", min(config$analysis_year), "-01-01'")
  after_date <- paste0("DATE '", max(config$analysis_year) + 1L, "-01-01'")
  dx_identifier <- qualified_identifier(config$komodo_schema, config$normalized_dx_table)
  procedure_identifier <- qualified_identifier(
    config$komodo_schema,
    config$normalized_procedure_table
  )
  candidate_identifier <- qualified_identifier(config$write_schema, config$dx_candidate_table)
  procedure_presence_identifier <- qualified_identifier(
    config$write_schema,
    config$cfi_procedure_presence_table
  )
  cfi_matches_identifier <- qualified_identifier(
    config$write_schema,
    config$cfi_feature_matches_table
  )
  gagne_matches_identifier <- qualified_identifier(
    config$write_schema,
    config$gagne_group_matches_table
  )

  ids_stage <- quote_identifier("share_metric_ids_stage")
  dx_lookup_stage <- quote_identifier("share_cfi_gagne_dx_lookup_stage")
  dx_prefix_stage <- quote_identifier("share_cfi_gagne_dx_prefix_stage")
  dx_candidate_raw <- quote_identifier("share_cfi_gagne_dx_candidate_raw")
  dx_distinct_codes <- quote_identifier("share_cfi_gagne_distinct_dx_codes")
  dx_feature_map <- quote_identifier("share_cfi_gagne_dx_feature_map")
  procedure_lookup_stage <- quote_identifier("share_cfi_procedure_lookup_stage")
  procedure_distinct_codes <- quote_identifier("share_cfi_distinct_procedure_codes")
  procedure_feature_map <- quote_identifier("share_cfi_procedure_feature_map")

  execute_sql(
    con,
    paste0(
      "CREATE TEMP TABLE ", ids_stage, "\n",
      "DISTKEY(patient_id) SORTKEY(patient_id) AS\n",
      "SELECT patid, patient_id, analysis_year, analysis_start_date, analysis_end_date\n",   # CHANGED: added the 2 dates
      "FROM ", ids_identifier, " WHERE analysis_year IN (", year_list, ");\n\n",   # CHANGED: was = 2022
      "CREATE TEMP TABLE ", dx_lookup_stage, " (\n",
      "  lookup_row_id INTEGER, lookup_version VARCHAR(128), metric VARCHAR(16),\n",
      "  feature_id VARCHAR(128), feature_name VARCHAR(256), match_value VARCHAR(64),\n",
      "  range_start VARCHAR(64), range_end VARCHAR(64),\n",
      "  range_end_inclusive VARCHAR(16), match_type VARCHAR(16),\n",
      "  candidate_prefix VARCHAR(16), candidate_prefix_length INTEGER\n",
      ") DISTSTYLE ALL SORTKEY(candidate_prefix_length, candidate_prefix);\n\n",
      "CREATE TEMP TABLE ", dx_prefix_stage, " (\n",
      "  candidate_prefix VARCHAR(16), candidate_prefix_length INTEGER\n",
      ") DISTSTYLE ALL SORTKEY(candidate_prefix_length, candidate_prefix);\n\n",
      "CREATE TEMP TABLE ", procedure_lookup_stage, " (\n",
      "  lookup_version VARCHAR(128), metric VARCHAR(16), feature_id VARCHAR(128),\n",
      "  feature_name VARCHAR(256), range_start VARCHAR(64), range_end VARCHAR(64),\n",
      "  match_type VARCHAR(16)\n",
      ") DISTSTYLE ALL SORTKEY(range_start, range_end);"
    )
  )

  diagnosis_insert <- diagnosis_lookup
  diagnosis_insert$lookup_row_id <- as.character(diagnosis_insert$lookup_row_id)
  diagnosis_insert$candidate_prefix_length <- as.character(
    diagnosis_insert$candidate_prefix_length
  )
  insert_data_frame(
    con,
    dx_lookup_stage,
    c(
      "lookup_row_id", "lookup_version", "metric", "feature_id", "feature_name",
      "match_value", "range_start", "range_end", "range_end_inclusive",
      "match_type", "candidate_prefix", "candidate_prefix_length"
    ),
    diagnosis_insert,
    numeric_columns = c("lookup_row_id", "candidate_prefix_length")
  )
  prefixes <- unique(diagnosis_lookup[, c(
    "candidate_prefix", "candidate_prefix_length"
  )])
  prefixes$candidate_prefix_length <- as.character(prefixes$candidate_prefix_length)
  insert_data_frame(
    con,
    dx_prefix_stage,
    c("candidate_prefix", "candidate_prefix_length"),
    prefixes,
    numeric_columns = "candidate_prefix_length"
  )
  insert_data_frame(
    con,
    procedure_lookup_stage,
    names(procedure_lookup),
    procedure_lookup
  )

  prefix_lengths <- sort(unique(as.integer(prefixes$candidate_prefix_length)))
  scan_branches <- vapply(prefix_lengths, function(prefix_length) {
    paste0(
      "SELECT ids.patid, ids.analysis_year, dx.dx_code\n",
      "FROM ", ids_stage, " ids\n",
      "JOIN ", dx_identifier, " dx ON dx.patient_id = ids.patient_id\n",
      "JOIN ", dx_prefix_stage, " p\n",
      "  ON p.candidate_prefix_length = ", prefix_length, "\n",
      " AND p.candidate_prefix = LEFT(dx.dx_code, ", prefix_length, ")\n",
      "WHERE dx.event_date >= ", first_date, "\n",                 # CHANGED: was DATE '2022-01-01'
      "  AND dx.event_date < ", after_date, "\n",                   # CHANGED: was DATE '2023-01-01'
      "  AND dx.event_date >= ids.analysis_start_date\n",          # CHANGED: only this row's year
      "  AND dx.event_date < ids.analysis_end_date + 1\n",         # CHANGED: only this row's year
      "  AND dx.dx_code IS NOT NULL AND dx.dx_code <> ''"
    )
  }, character(1))

  phase_started <- start_phase("Scan normalized diagnoses for CFI/Gagne candidates")
  execute_sql(
    con,
    paste0(
      "CREATE TEMP TABLE ", dx_candidate_raw, " (\n",
      "  patid VARCHAR(256), analysis_year INTEGER, dx_code VARCHAR(64)\n",
      ") DISTKEY(patid) SORTKEY(analysis_year, dx_code, patid);\n",
      "INSERT INTO ", dx_candidate_raw, "\n",
      paste(scan_branches, collapse = "\nUNION ALL\n"), ";\n",
      "CREATE TABLE ", candidate_identifier, "\n",
      "DISTKEY(patid) SORTKEY(analysis_year, dx_code, patid) AS\n",
      "SELECT DISTINCT patid, analysis_year, dx_code FROM ", dx_candidate_raw, ";"
    )
  )
  finish_phase("Scan normalized diagnoses for CFI/Gagne candidates", phase_started)

  candidate_qa <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*)::BIGINT AS n_rows,\n",
      "       SUM(CASE WHEN patid IS NULL OR dx_code IS NULL THEN 1 ELSE 0 END)::BIGINT AS n_bad\n",
      "FROM ", candidate_identifier
    )
  )
  if (candidate_qa$n_rows[[1]] == 0 || candidate_qa$n_bad[[1]] != 0) {
    stop("CFI/Gagne diagnosis candidate table failed integrity checks.")
  }

  phase_started <- start_phase("Scan normalized procedures for CFI-relevant codes")
  execute_sql(
    con,
    paste0(
      "CREATE TABLE ", procedure_presence_identifier, "\n",
      "DISTKEY(patid) SORTKEY(analysis_year, procedure_code, patid) AS\n",
      "SELECT DISTINCT ids.patid, ids.analysis_year, px.procedure_code\n",
      "FROM ", ids_stage, " ids\n",
      "JOIN ", procedure_identifier, " px ON px.patient_id = ids.patient_id\n",
      "JOIN ", procedure_lookup_stage, " l\n",
      "  ON px.procedure_code >= l.range_start\n",
      " AND px.procedure_code <= l.range_end\n",
      "WHERE px.event_date >= ", first_date, "\n",                 # CHANGED: was DATE '2022-01-01'
      "  AND px.event_date < ", after_date, "\n",                   # CHANGED: was DATE '2023-01-01'
      "  AND px.event_date >= ids.analysis_start_date\n",          # CHANGED: only this row's year
      "  AND px.event_date < ids.analysis_end_date + 1\n",         # CHANGED: only this row's year
      "  AND px.procedure_code IS NOT NULL AND px.procedure_code <> '';"
    )
  )
  finish_phase("Scan normalized procedures for CFI-relevant codes", phase_started)

  execute_sql(
    con,
    paste0(
      "CREATE TABLE ", cfi_matches_identifier, " (\n",
      "  patid VARCHAR(256), analysis_year INTEGER, metric VARCHAR(16),\n",
      "  feature_id VARCHAR(128), feature_name VARCHAR(256), match_type VARCHAR(16),\n",
      "  lookup_version VARCHAR(128)\n",
      ") DISTKEY(patid) SORTKEY(analysis_year, patid, feature_id);\n\n",
      "CREATE TABLE ", gagne_matches_identifier, " (\n",
      "  patid VARCHAR(256), analysis_year INTEGER, metric VARCHAR(16),\n",
      "  feature_id VARCHAR(128), feature_name VARCHAR(256), match_type VARCHAR(16),\n",
      "  lookup_version VARCHAR(128)\n",
      ") DISTKEY(patid) SORTKEY(analysis_year, patid, feature_id);"
    )
  )

  phase_started <- start_phase("Map distinct diagnosis and procedure codes to features")
  execute_sql(
    con,
    paste0(
      "CREATE TEMP TABLE ", dx_distinct_codes, " DISTSTYLE ALL SORTKEY(dx_code) AS\n",
      "SELECT DISTINCT dx_code FROM ", candidate_identifier, ";\n\n",
      "CREATE TEMP TABLE ", dx_feature_map, " DISTSTYLE ALL\n",
      "SORTKEY(dx_code, metric, feature_id) AS\n",
      "SELECT DISTINCT d.dx_code, l.metric, l.feature_id, l.feature_name,\n",
      "       l.match_type, l.lookup_version\n",
      "FROM ", dx_distinct_codes, " d\n",
      "JOIN ", dx_lookup_stage, " l\n",
      "  ON LEFT(d.dx_code, l.candidate_prefix_length) = l.candidate_prefix\n",
      " AND (\n",
      "   (l.match_type = 'exact' AND d.dx_code = l.match_value)\n",
      "   OR (l.match_type = 'prefix' AND LEFT(d.dx_code, LENGTH(l.match_value)) = l.match_value)\n",
      "   OR (l.match_type = 'range' AND d.dx_code >= l.range_start AND\n",
      "       ((COALESCE(UPPER(l.range_end_inclusive), 'TRUE') = 'FALSE' AND d.dx_code < l.range_end)\n",
      "        OR (COALESCE(UPPER(l.range_end_inclusive), 'TRUE') <> 'FALSE' AND d.dx_code <= l.range_end)))\n",
      " );\n\n",
      "INSERT INTO ", cfi_matches_identifier, "\n",
      "SELECT DISTINCT c.patid, c.analysis_year, m.metric, m.feature_id,\n",
      "       m.feature_name, m.match_type, m.lookup_version\n",
      "FROM ", candidate_identifier, " c\n",
      "JOIN ", dx_feature_map, " m ON m.dx_code = c.dx_code\n",
      "WHERE m.metric = 'CFI';\n\n",
      "INSERT INTO ", gagne_matches_identifier, "\n",
      "SELECT DISTINCT c.patid, c.analysis_year, m.metric, m.feature_id,\n",
      "       m.feature_name, m.match_type, m.lookup_version\n",
      "FROM ", candidate_identifier, " c\n",
      "JOIN ", dx_feature_map, " m ON m.dx_code = c.dx_code\n",
      "WHERE m.metric = 'GAGNE';\n\n",
      "CREATE TEMP TABLE ", procedure_distinct_codes, "\n",
      "DISTSTYLE ALL SORTKEY(procedure_code) AS\n",
      "SELECT DISTINCT procedure_code FROM ", procedure_presence_identifier, ";\n\n",
      "CREATE TEMP TABLE ", procedure_feature_map, " DISTSTYLE ALL\n",
      "SORTKEY(procedure_code, feature_id) AS\n",
      "SELECT DISTINCT p.procedure_code, l.metric, l.feature_id, l.feature_name,\n",
      "       l.match_type, l.lookup_version\n",
      "FROM ", procedure_distinct_codes, " p\n",
      "JOIN ", procedure_lookup_stage, " l\n",
      "  ON p.procedure_code >= l.range_start AND p.procedure_code <= l.range_end;\n\n",
      "INSERT INTO ", cfi_matches_identifier, "\n",
      "SELECT DISTINCT p.patid, p.analysis_year, m.metric, m.feature_id,\n",
      "       m.feature_name, m.match_type, m.lookup_version\n",
      "FROM ", procedure_presence_identifier, " p\n",
      "JOIN ", procedure_feature_map, " m ON m.procedure_code = p.procedure_code;"
    )
  )
  finish_phase("Map distinct diagnosis and procedure codes to features", phase_started)

  duplicate_qa <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT 'CFI' AS metric, COUNT(*) - COUNT(DISTINCT patid || '|' || feature_id || '|' || match_type || '|' || lookup_version) AS duplicate_rows\n",
      "FROM ", cfi_matches_identifier, "\n",
      "UNION ALL\n",
      "SELECT 'GAGNE', COUNT(*) - COUNT(DISTINCT patid || '|' || feature_id || '|' || match_type || '|' || lookup_version)\n",
      "FROM ", gagne_matches_identifier
    )
  )
  if (any(duplicate_qa$duplicate_rows != 0)) {
    stop("CFI or Gagne feature matches contain duplicate rows.")
  }

  print_aggregate_query(
    con,
    "CFI/Gagne feature-match QA:",
    paste0(
      "SELECT metric, match_type, COUNT(*)::BIGINT AS matched_rows FROM ",
      cfi_matches_identifier, " GROUP BY metric, match_type\nUNION ALL\n",
      "SELECT metric, match_type, COUNT(*)::BIGINT AS matched_rows FROM ",
      gagne_matches_identifier, " GROUP BY metric, match_type\n",
      "ORDER BY metric, match_type"
    )
  )
  invisible(output_tables)
}

run_prepare_cfi_gagne_evidence()
