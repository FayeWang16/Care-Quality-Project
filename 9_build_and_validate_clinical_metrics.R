source("Code/shareable_clinical_metrics/1_configure_clinical_metrics.R")

# Project: Care Quality (multimorbidity care quality in people with and without HIV), 2016-2025
# Adapted from Nemo Zhou's Frailty_Komoto shareable clinical metrics code
# Author: Lifei Wang
# Acknowledgements: Nemo Zhou
# Date started: 2026-10-12
# Date last updated: 2026-10-12
#
# ---- Adapted for the Care Quality project ----
# CHANGED: builds the final table for every year in config$analysis_year (2016-2025).
# CHANGED: Nemo's HIV status (File 8) is not used, so every HIV part is commented
#   out (marked "# HIV OFF"). HIV status comes from our own input table instead:
#   ps_matched has the column hiv (1 = HIV group, 0 = matched control), and it is
#   carried into the final table by "e.*" (all columns of ps_matched).
#
# ---- Purpose ----
# Join CFI, Gagne, CCW-30, and HIV outputs back to the existing 2022 eligibility
# table. The final table retains the eligibility fields and adds the metric
# outputs while preserving one row per eligible patient-year. This script also
# performs aggregate denominator, missingness, duplicate, and count-reconciliation
# checks. It does not export or print patient-level records.


qualified_column_selects <- function(alias, columns) {
  paste0(alias, ".", vapply(columns, quote_identifier, character(1)))
}

run_build_and_validate_clinical_metrics <- function() {
  config <- get_shareable_clinical_metrics_config()
  con <- connect_shareable_komodo(config)
  on.exit(disconnect_shareable_komodo(con), add = TRUE)

  required_tables <- list(
    metric_ids_table = c("patid", "patient_id", "analysis_year"),
    cfi_scores_table = c(
      "patid", "analysis_year", "cfi_score", "cfi_feature_count",
      "cfi_intercept_only_flag", "cfi_weight_sum", "lookup_version"
    ),
    gagne_scores_table = c(
      "patid", "analysis_year", "gagne_group_count", "gagne_score", "lookup_version"
    ),
    ccw30_indicators_table = c(
      "patid", "analysis_year", "ccw_cancer", "ccw_condition_count", "lookup_version"
    ),
    ccw30_counts_table = c(
      "patid", "analysis_year", "ccw_total_condition_count", "lookup_version"
    )
    # HIV OFF: Nemo's HIV status table (File 8) is not needed
    # hiv_status_table = c(
    #   "patid", "analysis_year", "hiv_status", "hiv_inpatient_evidence",
    #   "hiv_non_inpatient_distinct_dates", "hiv_non_inpatient_second_date",
    #   "hiv_first_observed_date", "hiv_evidence_count", "lookup_version"
    # )
  )
  for (field in names(required_tables)) {
    require_table_columns(
      con,
      config$write_schema,
      config[[field]],
      required_tables[[field]]
    )
  }
  require_table_columns(
    con,
    config$write_schema,
    config$eligibility_table,
    c("patient_id", "analysis_year", "hiv")   # CHANGED: also needs our hiv column
  )

  prepare_output_table(con, config, config$final_table)
  eligibility_identifier <- qualified_identifier(
    config$write_schema,
    config$eligibility_table
  )
  ids_identifier <- qualified_identifier(config$write_schema, config$metric_ids_table)
  cfi_identifier <- qualified_identifier(config$write_schema, config$cfi_scores_table)
  gagne_identifier <- qualified_identifier(config$write_schema, config$gagne_scores_table)
  ccw_identifier <- qualified_identifier(config$write_schema, config$ccw30_indicators_table)
  ccw_counts_identifier <- qualified_identifier(
    config$write_schema,
    config$ccw30_counts_table
  )
  # hiv_identifier <- qualified_identifier(config$write_schema, config$hiv_status_table)   # HIV OFF
  year_list <- paste(config$analysis_year, collapse = ", ")   # CHANGED: e.g. "2016, 2017, ..., 2025"
  final_identifier <- qualified_identifier(config$write_schema, config$final_table)

  ccw_columns <- setdiff(
    get_table_columns(con, config$write_schema, config$ccw30_indicators_table),
    c("patid", "patient_id", "analysis_year", "lookup_version")
  )
  gagne_columns <- setdiff(
    get_table_columns(con, config$write_schema, config$gagne_scores_table),
    c("patid", "patient_id", "analysis_year", "lookup_version")
  )
  select_columns <- c(
    "ids.patid",
    "e.*",
    "cfi.cfi_score",
    "cfi.cfi_feature_count",
    "cfi.cfi_intercept_only_flag",
    "cfi.cfi_weight_sum",
    "cfi.lookup_version AS cfi_lookup_version",
    qualified_column_selects("ccw", ccw_columns),
    "ccw.lookup_version AS ccw30_lookup_version",
    "ccw_count.ccw_total_condition_count",
    qualified_column_selects("gagne", gagne_columns),
    "gagne.lookup_version AS gagne_lookup_version"   # HIV OFF: the comma and the hiv columns below were removed
    # "hiv.hiv_status",
    # "hiv.hiv_inpatient_evidence",
    # "hiv.hiv_non_inpatient_distinct_dates",
    # "hiv.hiv_non_inpatient_second_date",
    # "hiv.hiv_first_observed_date",
    # "hiv.hiv_evidence_count",
    # "hiv.lookup_version AS hiv_lookup_version"
  )

  phase_started <- start_phase("Build combined clinical metrics table")
  execute_sql(
    con,
    paste0(
      "CREATE TABLE ", final_identifier, "\n",
      "DISTKEY(patient_id) SORTKEY(analysis_year, patient_id) AS\n",
      "SELECT\n  ", paste(select_columns, collapse = ",\n  "), "\n",
      "FROM ", ids_identifier, " ids\n",
      "JOIN ", eligibility_identifier, " e\n",
      "  ON e.patient_id = ids.patient_id AND e.analysis_year = ids.analysis_year\n",
      "LEFT JOIN ", cfi_identifier, " cfi\n",
      "  ON cfi.patid = ids.patid AND cfi.analysis_year = ids.analysis_year\n",
      "LEFT JOIN ", ccw_identifier, " ccw\n",
      "  ON ccw.patid = ids.patid AND ccw.analysis_year = ids.analysis_year\n",
      "LEFT JOIN ", ccw_counts_identifier, " ccw_count\n",
      "  ON ccw_count.patid = ids.patid AND ccw_count.analysis_year = ids.analysis_year\n",
      "LEFT JOIN ", gagne_identifier, " gagne\n",
      "  ON gagne.patid = ids.patid AND gagne.analysis_year = ids.analysis_year\n",
      # "LEFT JOIN ", hiv_identifier, " hiv\n",                                          # HIV OFF
      # "  ON hiv.patid = ids.patid AND hiv.analysis_year = ids.analysis_year\n",        # HIV OFF
      "WHERE ids.analysis_year IN (", year_list, ");"   # CHANGED: was = 2022
    )
  )
  finish_phase("Build combined clinical metrics table", phase_started)

  check_one_row_per_patient_year(con, final_identifier, "Combined clinical metrics table")
  qa <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(ids.patid)::BIGINT AS denominator_rows,\n",
      "       COUNT(final.patid)::BIGINT AS final_rows,\n",
      "       SUM(CASE WHEN final.cfi_score IS NULL THEN 1 ELSE 0 END)::BIGINT AS missing_cfi,\n",
      "       SUM(CASE WHEN final.ccw_condition_count IS NULL THEN 1 ELSE 0 END)::BIGINT AS missing_ccw30,\n",
      "       SUM(CASE WHEN final.gagne_score IS NULL THEN 1 ELSE 0 END)::BIGINT AS missing_gagne,\n",
      "       SUM(CASE WHEN final.hiv IS NULL THEN 1 ELSE 0 END)::BIGINT AS missing_hiv,\n",   # CHANGED: our hiv column
      "       SUM(CASE WHEN final.ccw_condition_count <> final.ccw_total_condition_count\n",
      "                THEN 1 ELSE 0 END)::BIGINT AS inconsistent_ccw_counts\n",
      "FROM ", ids_identifier, " ids\n",
      "LEFT JOIN ", final_identifier, " final ON final.patid = ids.patid"
    )
  )
  print(qa)
  final_problem_columns <- c(
    "missing_cfi", "missing_ccw30", "missing_gagne", "missing_hiv",
    "inconsistent_ccw_counts"
  )
  if (
    qa$denominator_rows[[1]] != qa$final_rows[[1]] ||
      any(unlist(qa[1, final_problem_columns], use.names = FALSE) != 0)
  ) {
    stop("Combined clinical metrics table failed final integrity checks.")
  }

  print_aggregate_query(
    con,
    "Final metric range and prevalence QA:",
    paste0(
      "SELECT MIN(cfi_score) AS min_cfi, MAX(cfi_score) AS max_cfi,\n",
      "       MIN(gagne_score) AS min_gagne, MAX(gagne_score) AS max_gagne,\n",
      "       MIN(ccw_condition_count) AS min_ccw30_count,\n",
      "       MAX(ccw_condition_count) AS max_ccw30_count,\n",
      "       SUM(CASE WHEN hiv = 1 THEN 1 ELSE 0 END)::BIGINT AS hiv_group_rows,\n",      # CHANGED: our hiv column
      "       SUM(CASE WHEN hiv = 0 THEN 1 ELSE 0 END)::BIGINT AS control_rows\n",          # CHANGED: added
      "FROM ", final_identifier
    )
  )
  message("Created ", config$write_schema, ".", config$final_table, ".")
  invisible(config$final_table)
}

run_build_and_validate_clinical_metrics()
