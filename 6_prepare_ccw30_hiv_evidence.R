source("Code/shareable_clinical_metrics/1_configure_clinical_metrics.R")

# Project: Care Quality (multimorbidity care quality in people with and without HIV), 2016-2025
# Adapted from Nemo Zhou's Frailty_Komoto shareable clinical metrics code
# Author: Lifei Wang
# Acknowledgements: Nemo Zhou
# Date started: 2026-10-12
# Date last updated: 2026-10-12
#
# ---- Adapted for the Care Quality project ----
# CHANGED: works for every year in config$analysis_year (2016-2025), not only 2022:
#   - keeps the years in config$analysis_year (was only 2022)
#   - each row only uses diagnoses from its own year: analysis_start_date to
#     analysis_end_date from File 2 (was always 2022-01-01 to 2022-12-31)
#   - an extra fixed date range (first Jan 1 to last Dec 31) is kept so Redshift
#     can still skip records outside 2016-2025 quickly
# CHANGED: the HIV evidence part is commented out (marked "# HIV OFF"), so no
#   cq_hiv_diagnosis_evidence table is made. Our HIV group comes from our own
#   protocol rules (Files 04-07), so Nemo's HIV table would only cause confusion.
#
# ---- Purpose ----
# Match normalized 2022 diagnoses to the supplied CCW-30 and HIV exact-code
# lookups in one Redshift scan. CCW-30 evidence retains the condition and
# qualification-group metadata needed by script 7. HIV evidence retains the
# diagnosis date and inpatient/non-inpatient setting needed by script 8.
#
# The supplied BPH and stroke/TIA encounter-level exclusions remain explicitly
# omitted, as in the active 3.8a implementation, because normalized_dx_events
# does not contain a reliable claim identifier for applying those exclusions.


run_prepare_ccw30_hiv_evidence <- function() {
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
    config$komodo_schema,
    config$normalized_dx_table,
    c("patient_id", "event_date", "source_table", "dx_code")
  )

  ccw_lookup <- read_lookup_csv(file.path(
    config$ccw30_lookup_dir,
    "0.6_ccw30_diagnosis_lookup.csv"
  ))
  # HIV OFF: not needed (we use our own HIV rules)
#   hiv_lookup <- read_lookup_csv(file.path(
#     config$lookup_dir,
#     "0.6_hiv_diagnosis_lookup.csv"
#   ))
  require_columns(
    ccw_lookup,
    c(
      "lookup_version", "condition_name", "condition_name_long",
      "ccw_condition_id", "qualification_group", "rule_type", "dx_code",
      "match_type", "exclusion_implementation"
    ),
    "CCW-30 diagnosis lookup"
  )
  # HIV OFF
#   require_columns(
#     hiv_lookup,
#     c(
#       "lookup_version", "metric", "feature_id", "feature_name", "code_system",
#       "match_value", "match_type"
#     ),
#     "HIV diagnosis lookup"
#   )

  ccw_lookup$match_type <- tolower(ccw_lookup$match_type)
  ccw_lookup$rule_type <- tolower(ccw_lookup$rule_type)
  ccw_lookup$qualification_group <- as.integer(ccw_lookup$qualification_group)
  ccw_inclusions <- ccw_lookup[
    !is.na(ccw_lookup$rule_type) & ccw_lookup$rule_type == "inclusion",
    c(
      "lookup_version", "condition_name", "condition_name_long",
      "ccw_condition_id", "qualification_group", "dx_code", "match_type"
    )
  ]
  if (nrow(ccw_inclusions) == 0L || any(ccw_inclusions$match_type != "exact")) {
    stop("CCW-30 inclusion lookup must contain active exact-code rows.")
  }

  ccw_exclusions <- ccw_lookup[ccw_lookup$rule_type == "exclusion", ]
  expected_exclusion_conditions <- c("bph", "stroketia")
  if (
    !identical(sort(unique(ccw_exclusions$condition_name)), expected_exclusion_conditions) ||
      !all(grepl(
        "^omitted_no_reliable_claim_identifier",
        ccw_exclusions$exclusion_implementation
      ))
  ) {
    stop("CCW-30 BPH/stroke-TIA exclusion metadata does not match the active contract.")
  }

  # HIV OFF
#   hiv_lookup$metric <- toupper(hiv_lookup$metric)
#   hiv_lookup$match_type <- tolower(hiv_lookup$match_type)
#   hiv_inclusions <- hiv_lookup[
#     !is.na(hiv_lookup$metric) & hiv_lookup$metric == "HIV" &
#       !is.na(hiv_lookup$code_system) & hiv_lookup$code_system == "ICD10CM" &
#       !is.na(hiv_lookup$match_type) & hiv_lookup$match_type == "exact" &
#       !is.na(hiv_lookup$match_value) & nzchar(hiv_lookup$match_value),
#     c("lookup_version", "metric", "feature_id", "feature_name", "match_value", "match_type")
#   ]
#   if (nrow(hiv_inclusions) == 0L) {
#     stop("HIV lookup contains no active exact ICD-10-CM codes.")
#   }

  prepare_output_table(con, config, config$ccw30_evidence_table)
  # prepare_output_table(con, config, config$hiv_evidence_table)   # HIV OFF

  ids_identifier <- qualified_identifier(config$write_schema, config$metric_ids_table)
  # CHANGED: years and the overall date range, e.g. 2016-01-01 to 2026-01-01
  year_list  <- paste(config$analysis_year, collapse = ", ")
  first_date <- paste0("DATE '", min(config$analysis_year), "-01-01'")
  after_date <- paste0("DATE '", max(config$analysis_year) + 1L, "-01-01'")
  dx_identifier <- qualified_identifier(config$komodo_schema, config$normalized_dx_table)
  ccw_identifier <- qualified_identifier(config$write_schema, config$ccw30_evidence_table)
  # hiv_identifier <- qualified_identifier(config$write_schema, config$hiv_evidence_table)   # HIV OFF
  ids_stage <- quote_identifier("share_ccw30_hiv_ids_stage")
  lookup_stage <- quote_identifier("share_ccw30_hiv_lookup_stage")
  shared_evidence <- quote_identifier("share_ccw30_hiv_evidence_stage")

  # HIV OFF: only the CCW-30 rows now (Nemo also added HIV rows here)
  shared_lookup <- rbind(
    data.frame(
      metric = "CCW30",
      lookup_version = ccw_inclusions$lookup_version,
      feature_id = ccw_inclusions$ccw_condition_id,
      feature_name = ccw_inclusions$condition_name_long,
      condition_name = ccw_inclusions$condition_name,
      condition_name_long = ccw_inclusions$condition_name_long,
      ccw_condition_id = ccw_inclusions$ccw_condition_id,
      qualification_group = ccw_inclusions$qualification_group,
      dx_code = ccw_inclusions$dx_code,
      match_type = ccw_inclusions$match_type,
      stringsAsFactors = FALSE
    )   # HIV OFF: the comma and the HIV data.frame below were removed
#     data.frame(
#       metric = hiv_inclusions$metric,
#       lookup_version = hiv_inclusions$lookup_version,
#       feature_id = hiv_inclusions$feature_id,
#       feature_name = hiv_inclusions$feature_name,
#       condition_name = NA_character_,
#       condition_name_long = NA_character_,
#       ccw_condition_id = NA_character_,
#       qualification_group = NA_integer_,
#       dx_code = hiv_inclusions$match_value,
#       match_type = hiv_inclusions$match_type,
#       stringsAsFactors = FALSE
#     )
  )

  execute_sql(
    con,
    paste0(
      "CREATE TEMP TABLE ", ids_stage, "\n",
      "DISTKEY(patient_id) SORTKEY(patient_id) AS\n",
      "SELECT patid, patient_id, analysis_year, analysis_start_date, analysis_end_date\n",   # CHANGED: added the 2 dates
      "FROM ", ids_identifier, " WHERE analysis_year IN (", year_list, ");\n\n",   # CHANGED: was = 2022
      "CREATE TEMP TABLE ", lookup_stage, " (\n",
      "  metric VARCHAR(16), lookup_version VARCHAR(128), feature_id VARCHAR(128),\n",
      "  feature_name VARCHAR(256), condition_name VARCHAR(64),\n",
      "  condition_name_long VARCHAR(256), ccw_condition_id VARCHAR(128),\n",
      "  qualification_group INTEGER, dx_code VARCHAR(64), match_type VARCHAR(16)\n",
      ") DISTSTYLE ALL SORTKEY(dx_code);"
    )
  )
  shared_lookup$qualification_group <- as.character(shared_lookup$qualification_group)
  insert_data_frame(
    con,
    lookup_stage,
    names(shared_lookup),
    shared_lookup,
    numeric_columns = "qualification_group"
  )

  phase_started <- start_phase("Scan normalized diagnoses for CCW-30 evidence")
  execute_sql(
    con,
    paste0(
      "CREATE TEMP TABLE ", shared_evidence, "\n",
      "DISTKEY(patid) SORTKEY(analysis_year, patid, event_date) AS\n",
      "SELECT DISTINCT\n",
      "  ids.patid, ids.patient_id, ids.analysis_year, l.metric, l.feature_id,\n",
      "  l.feature_name, l.condition_name, l.condition_name_long,\n",
      "  l.ccw_condition_id, l.qualification_group,\n",
      "  dx.event_date::DATE AS event_date, dx.source_table,\n",
      "  CASE dx.source_table\n",
      "    WHEN 'INPATIENT_EVENTS' THEN 'inpatient'\n",
      "    WHEN 'NON_INPATIENT_EVENTS' THEN 'outpatient'\n",
      "  END AS claim_setting,\n",
      "  dx.dx_code, l.match_type, l.lookup_version\n",
      "FROM ", ids_stage, " ids\n",
      "JOIN ", dx_identifier, " dx ON dx.patient_id = ids.patient_id\n",
      "JOIN ", lookup_stage, " l ON l.dx_code = dx.dx_code\n",
      "WHERE dx.event_date >= ", first_date, "\n",                 # CHANGED: was DATE '2022-01-01'
      "  AND dx.event_date < ", after_date, "\n",                   # CHANGED: was DATE '2023-01-01'
      "  AND dx.event_date >= ids.analysis_start_date\n",          # CHANGED: only this row's year
      "  AND dx.event_date < ids.analysis_end_date + 1\n",         # CHANGED: only this row's year
      "  AND dx.source_table IN ('INPATIENT_EVENTS', 'NON_INPATIENT_EVENTS');\n\n",
      "CREATE TABLE ", ccw_identifier, "\n",
      "DISTKEY(patid) SORTKEY(analysis_year, patid, event_date) AS\n",
      "SELECT patid, patient_id, analysis_year, metric, feature_id, feature_name,\n",
      "       event_date, source_table, claim_setting, dx_code, match_type,\n",
      "       lookup_version, condition_name, condition_name_long,\n",
      "       ccw_condition_id, qualification_group\n",
      "FROM ", shared_evidence, " WHERE metric = 'CCW30';"   # HIV OFF: the HIV table below is not made
#       "CREATE TABLE ", hiv_identifier, "\n",
#       "DISTKEY(patid) SORTKEY(analysis_year, patid, diagnosis_date) AS\n",
#       "SELECT patid, analysis_year, event_date AS diagnosis_date,\n",
#       "       CASE WHEN claim_setting = 'inpatient' THEN 'inpatient'\n",
#       "            ELSE 'non_inpatient' END AS claim_setting,\n",
#       "       dx_code, metric, feature_id, feature_name, match_type, lookup_version\n",
#       "FROM ", shared_evidence, " WHERE metric = 'HIV';"
    )
  )
  finish_phase("Scan normalized diagnoses for CCW-30 evidence", phase_started)

  duplicate_qa <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT 'CCW30' AS metric, COUNT(*) - COUNT(DISTINCT patid || '|' || event_date::VARCHAR || '|' || dx_code || '|' || feature_id || '|' || source_table) AS duplicate_rows\n",
      "FROM ", ccw_identifier   # HIV OFF: the HIV duplicate check below is not run
      # "\nUNION ALL\n",
      # "SELECT 'HIV', COUNT(*) - COUNT(DISTINCT patid || '|' || diagnosis_date::VARCHAR || '|' || dx_code || '|' || claim_setting)\n",
      # "FROM ", hiv_identifier
    )
  )
  if (any(duplicate_qa$duplicate_rows != 0)) {
    stop("CCW-30 evidence contains duplicate rows.")
  }

  print_aggregate_query(
    con,
    "CCW-30 matched-evidence QA:",
    paste0(
      "SELECT 'CCW30' AS metric, source_table, claim_setting, COUNT(*)::BIGINT AS matched_rows\n",
      "FROM ", ccw_identifier, " GROUP BY source_table, claim_setting\n",   # HIV OFF: was "...\nUNION ALL\n"
      # "SELECT 'HIV', NULL AS source_table, claim_setting, COUNT(*)::BIGINT AS matched_rows\n",   # HIV OFF
      # "FROM ", hiv_identifier, " GROUP BY claim_setting\n",                                      # HIV OFF
      "ORDER BY metric, source_table, claim_setting"
    )
  )
  invisible(config$ccw30_evidence_table)   # HIV OFF: was c(ccw30, hiv)
}

run_prepare_ccw30_hiv_evidence()
