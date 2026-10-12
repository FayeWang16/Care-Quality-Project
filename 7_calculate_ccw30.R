source("Code/shareable_clinical_metrics/1_configure_clinical_metrics.R")

# Project: Care Quality (multimorbidity care quality in people with and without HIV), 2016-2025
# Adapted from Nemo Zhou's Frailty_Komoto shareable clinical metrics code
# Author: Lifei Wang
# Acknowledgements: Nemo Zhou
# Date started: 2026-10-12
# Date last updated: 2026-10-12
#
# ---- Adapted for the Care Quality project ----
# CHANGED: makes condition flags for every year in config$analysis_year (2016-2025),
#   not only 2022. The rules are unchanged, and each year is judged on its own
#   year's diagnoses (File 6 already keeps each row to its own year).
#
# ---- Purpose ----
# Apply the active CCW-30 qualification rules to the matched diagnosis evidence.
# Group 1 requires inpatient evidence; groups 2 and 3 require inpatient evidence
# or diagnoses on at least two distinct outpatient dates; group 4 requires
# outpatient evidence; and groups 5 and 6 accept either setting. The outputs are
# a qualified-condition long table and complete one-row-per-patient-year flags.


run_calculate_ccw30 <- function() {
  config <- get_shareable_clinical_metrics_config()
  con <- connect_shareable_komodo(config)
  on.exit(disconnect_shareable_komodo(con), add = TRUE)

  require_table_columns(
    con,
    config$write_schema,
    config$metric_ids_table,
    c("patid", "patient_id", "analysis_year")
  )
  require_table_columns(
    con,
    config$write_schema,
    config$ccw30_evidence_table,
    c(
      "patid", "patient_id", "analysis_year", "event_date", "claim_setting",
      "condition_name", "condition_name_long", "ccw_condition_id",
      "qualification_group", "lookup_version"
    )
  )

  metadata <- read_lookup_csv(file.path(
    config$ccw30_lookup_dir,
    "0.6_ccw30_condition_metadata.csv"
  ))
  require_columns(
    metadata,
    c(
      "condition_name", "condition_name_long", "ccw_condition_id",
      "qualification_group", "lookup_version"
    ),
    "CCW-30 condition metadata"
  )
  metadata$qualification_group <- as.integer(metadata$qualification_group)
  if (
    nrow(metadata) != 30L ||
      length(unique(metadata$ccw_condition_id)) != 30L ||
      any(!metadata$qualification_group %in% 1:6)
  ) {
    stop("CCW-30 metadata must contain 30 unique conditions in groups 1-6.")
  }
  lookup_versions <- unique(metadata$lookup_version)
  if (length(lookup_versions) != 1L || is.na(lookup_versions)) {
    stop("CCW-30 metadata must contain one nonmissing lookup version.")
  }

  output_tables <- c(
    config$ccw30_conditions_long_table,
    config$ccw30_indicators_table,
    config$ccw30_counts_table
  )
  for (table in output_tables) {
    prepare_output_table(con, config, table)
  }

  ids_identifier <- qualified_identifier(config$write_schema, config$metric_ids_table)
  year_list <- paste(config$analysis_year, collapse = ", ")   # CHANGED: e.g. "2016, 2017, ..., 2025"
  evidence_identifier <- qualified_identifier(
    config$write_schema,
    config$ccw30_evidence_table
  )
  long_identifier <- qualified_identifier(
    config$write_schema,
    config$ccw30_conditions_long_table
  )
  indicators_identifier <- qualified_identifier(
    config$write_schema,
    config$ccw30_indicators_table
  )
  counts_identifier <- qualified_identifier(
    config$write_schema,
    config$ccw30_counts_table
  )
  qualified_stage <- quote_identifier("share_ccw30_qualified_conditions")

  condition_ids <- metadata$ccw_condition_id
  indicator_columns <- paste0("ccw_", condition_ids)
  cancer_condition_ids <- condition_ids[grepl("^cancer_", condition_ids)]
  if (length(cancer_condition_ids) != 6L) {
    stop("Expected six site-specific CCW-30 cancer conditions.")
  }
  flag_sql <- vapply(condition_ids, function(condition_id) {
    paste0(
      "MAX(CASE WHEN q.ccw_condition_id = ", sql_string(condition_id),
      " THEN 1 ELSE 0 END)::INTEGER AS ",
      quote_identifier(paste0("ccw_", condition_id))
    )
  }, character(1))
  cancer_sql <- paste0(
    "MAX(CASE WHEN q.ccw_condition_id IN (",
    paste(vapply(cancer_condition_ids, sql_string, character(1)), collapse = ", "),
    ") THEN 1 ELSE 0 END)::INTEGER AS ccw_cancer"
  )

  phase_started <- start_phase("Apply CCW-30 qualification rules")
  execute_sql(
    con,
    paste0(
      "CREATE TEMP TABLE ", qualified_stage, "\n",
      "DISTKEY(patid) SORTKEY(analysis_year, patid, ccw_condition_id) AS\n",
      "SELECT patid, patient_id, analysis_year, condition_name, condition_name_long,\n",
      "       ccw_condition_id, qualification_group, lookup_version\n",
      "FROM ", evidence_identifier, "\n",
      "GROUP BY patid, patient_id, analysis_year, condition_name, condition_name_long,\n",
      "         ccw_condition_id, qualification_group, lookup_version\n",
      "HAVING (qualification_group = 1 AND\n",
      "          MAX(CASE WHEN claim_setting = 'inpatient' THEN 1 ELSE 0 END) = 1)\n",
      "    OR (qualification_group IN (2, 3) AND (\n",
      "          MAX(CASE WHEN claim_setting = 'inpatient' THEN 1 ELSE 0 END) = 1\n",
      "          OR COUNT(DISTINCT CASE WHEN claim_setting = 'outpatient' THEN event_date END) >= 2))\n",
      "    OR (qualification_group = 4 AND\n",
      "          MAX(CASE WHEN claim_setting = 'outpatient' THEN 1 ELSE 0 END) = 1)\n",
      "    OR (qualification_group IN (5, 6) AND COUNT(*) > 0);\n\n",
      "CREATE TABLE ", long_identifier, "\n",
      "DISTKEY(patid) SORTKEY(analysis_year, patid, ccw_condition_id) AS\n",
      "SELECT * FROM ", qualified_stage, ";\n\n",
      "CREATE TABLE ", indicators_identifier, "\n",
      "DISTKEY(patid) SORTKEY(analysis_year, patid) AS\n",
      "SELECT ids.patid, ids.patient_id, ids.analysis_year,\n",
      "       ", paste(flag_sql, collapse = ",\n       "), ",\n",
      "       ", cancer_sql, ",\n",
      "       COUNT(DISTINCT q.ccw_condition_id)::INTEGER AS ccw_condition_count,\n",
      "       ", sql_string(lookup_versions[[1]]), "::VARCHAR(128) AS lookup_version\n",
      "FROM ", ids_identifier, " ids\n",
      "LEFT JOIN ", qualified_stage, " q\n",
      "  ON q.patid = ids.patid AND q.analysis_year = ids.analysis_year\n",
      "WHERE ids.analysis_year IN (", year_list, ")\n",   # CHANGED: was = 2022
      "GROUP BY ids.patid, ids.patient_id, ids.analysis_year;\n\n",
      "CREATE TABLE ", counts_identifier, "\n",
      "DISTKEY(patid) SORTKEY(analysis_year, patid) AS\n",
      "SELECT patid, patient_id, analysis_year,\n",
      "       ccw_condition_count AS ccw_total_condition_count, lookup_version\n",
      "FROM ", indicators_identifier, ";"
    )
  )
  finish_phase("Apply CCW-30 qualification rules", phase_started)

  check_one_row_per_patient_year(con, indicators_identifier, "CCW-30 indicator table")
  check_one_row_per_patient_year(con, counts_identifier, "CCW-30 count table")
  flag_sum_sql <- paste(quote_identifier(indicator_columns), collapse = " + ")
  qa <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*)::BIGINT AS n_rows,\n",
      "       SUM(CASE WHEN ccw_condition_count <> (", flag_sum_sql,
      ") THEN 1 ELSE 0 END)::BIGINT AS inconsistent_counts,\n",
      "       SUM(CASE WHEN ccw_condition_count < 0 OR ccw_condition_count > 30\n",
      "                THEN 1 ELSE 0 END)::BIGINT AS invalid_counts\n",
      "FROM ", indicators_identifier
    )
  )
  print(qa)
  if (qa$inconsistent_counts[[1]] != 0 || qa$invalid_counts[[1]] != 0) {
    stop("CCW-30 indicators and condition counts do not reconcile.")
  }
  message(
    "CCW-30 complete. BPH and stroke/TIA encounter exclusions remain documented ",
    "but intentionally unapplied."
  )
  invisible(output_tables)
}

run_calculate_ccw30()
