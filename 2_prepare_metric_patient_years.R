source("Code/shareable_clinical_metrics/1_configure_clinical_metrics.R")

# Project: Frailty_Komoto shareable clinical metrics (2016-2025)
# Author: Lifei Wang
# Acknowledgements: Nemo Zhou
# Date started: 2026-10-12
# Date last updated: 2026-10-12
#
# ---- Adapted for the Care Quality project ----
# Changes from Nemo's version (everything else is unchanged):
#   - works for every year in config$analysis_year (2016-2025), not only 2022:
#   - patid = patient_id + "_" + that row's year (was always "_2022")
#   - analysis dates = Jan 1 to Dec 31 of that row's year (were always 2022)
#   - keeps the years in config$analysis_year (was only 2022)
#
# ---- Purpose ----
# Create the minimal patient-year denominator used by all four metric branches.
# This script does not construct eligibility. It reads the already-created 2022
# all-eligible comparison cohort and derives only the stable patient-year key and
# calendar-year analysis bounds needed for normalized event matching.


run_prepare_metric_patient_years <- function() {
  config <- get_shareable_clinical_metrics_config()
  con <- connect_shareable_komodo(config)
  on.exit(disconnect_shareable_komodo(con), add = TRUE)

  require_table_columns(
    con,
    config$write_schema,
    config$eligibility_table,
    c("patient_id", "analysis_year", "index_date")
  )
  prepare_output_table(con, config, config$metric_ids_table)

  eligibility_identifier <- qualified_identifier(
    config$write_schema,
    config$eligibility_table
  )
  ids_identifier <- qualified_identifier(config$write_schema, config$metric_ids_table)
  year_list <- paste(config$analysis_year, collapse = ", ")   # CHANGED: e.g. "2016, 2017, ..., 2025"

  phase_started <- start_phase("Build metric patient-years")
  execute_sql(
    con,
    paste0(
      "CREATE TABLE ", ids_identifier, "\n",
      "DISTKEY(patient_id)\n",
      "SORTKEY(analysis_year, patient_id) AS\n",
      "SELECT DISTINCT\n",
      "  e.patient_id || '_' || e.analysis_year AS patid,\n",   # CHANGED: was '_2022'
      "  e.patient_id,\n",
      "  e.analysis_year,\n",
      "  CAST(e.index_date AS DATE) AS eligibility_index_date,\n",
      "  TO_DATE(e.analysis_year || '-01-01', 'YYYY-MM-DD') AS analysis_start_date,\n",   # CHANGED: was 2022-01-01
      "  TO_DATE(e.analysis_year || '-12-31', 'YYYY-MM-DD') AS analysis_end_date\n",     # CHANGED: was 2022-12-31
      "FROM ", eligibility_identifier, " e\n",
      "WHERE e.analysis_year IN (", year_list, ")\n",   # CHANGED: was = 2022
      "  AND e.patient_id IS NOT NULL;"
    )
  )
  finish_phase("Build metric patient-years", phase_started)

  integrity <- check_one_row_per_patient_year(
    con,
    ids_identifier,
    "Shareable metric ID table"
  )
  source_count <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*)::BIGINT AS n_rows\n",
      "FROM ", eligibility_identifier, "\n",
      "WHERE analysis_year IN (", year_list, ") AND patient_id IS NOT NULL"   # CHANGED: was = 2022
    )
  )
  if (integrity$n_rows[[1]] != source_count$n_rows[[1]]) {
    stop("Metric ID row count does not equal the eligibility-table row count.")
  }

  print_aggregate_query(
    con,
    "Metric patient-year QA:",
    paste0(
      "SELECT analysis_year, COUNT(*)::BIGINT AS patient_years\n",
      "FROM ", ids_identifier, "\n",
      "GROUP BY analysis_year ORDER BY analysis_year"
    )
  )
  message("Created ", config$write_schema, ".", config$metric_ids_table, ".")
  invisible(config$metric_ids_table)
}

run_prepare_metric_patient_years()
