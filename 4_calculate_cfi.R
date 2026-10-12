source("Code/shareable_clinical_metrics/1_configure_clinical_metrics.R")

# Project: Care Quality (multimorbidity care quality in people with and without HIV), 2016-2025
# Adapted from Nemo Zhou's Frailty_Komoto shareable clinical metrics code
# Author: Lifei Wang
# Acknowledgements: Nemo Zhou
# Date started: 2026-10-12
# Date last updated: 2026-10-12
#
# ---- Adapted for the Care Quality project ----
# CHANGED: scores every year in config$analysis_year (2016-2025), not only 2022.
#
# ---- Purpose ----
# Calculate one CFI score per eligible 2022 patient-year from the compact CFI
# feature-match table. Each CFI disease number contributes at most once. The
# score is the supplied model intercept (0.10288 by default) plus the sum of the
# supplied disease weights. Patient-years without matched features receive the
# intercept-only score.



run_calculate_cfi <- function() {
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
    config$cfi_feature_matches_table,
    c("patid", "analysis_year", "metric", "feature_id", "lookup_version")
  )

  weight_lookup <- read_lookup_csv(file.path(
    config$lookup_dir,
    "0.6_cfi_weight_lookup.csv"
  ))
  required_columns <- c(
    "lookup_version", "metric", "disease_number", "feature_id", "weight"
  )
  require_columns(weight_lookup, required_columns, "CFI weight lookup")
  weight_lookup <- weight_lookup[
    toupper(weight_lookup$metric) == "CFI",
    required_columns
  ]
  weight_lookup$disease_number <- as.integer(weight_lookup$disease_number)
  weight_lookup$weight <- as.numeric(weight_lookup$weight)
  weight_lookup <- weight_lookup[
    !is.na(weight_lookup$disease_number) &
      !is.na(weight_lookup$weight) &
      !is.na(weight_lookup$feature_id) & nzchar(weight_lookup$feature_id),
  ]
  if (nrow(weight_lookup) == 0L) {
    stop("The CFI weight lookup contains no usable CFI weights.")
  }
  if (anyDuplicated(weight_lookup$feature_id)) {
    stop("The CFI weight lookup must contain one row per feature_id.")
  }

  prepare_output_table(con, config, config$cfi_scores_table)
  ids_identifier <- qualified_identifier(config$write_schema, config$metric_ids_table)
  matches_identifier <- qualified_identifier(
    config$write_schema,
    config$cfi_feature_matches_table
  )
  scores_identifier <- qualified_identifier(config$write_schema, config$cfi_scores_table)
  weight_stage <- quote_identifier("share_cfi_weight_stage")
  year_list <- paste(config$analysis_year, collapse = ", ")   # CHANGED: e.g. "2016, 2017, ..., 2025"

  execute_sql(
    con,
    paste0(
      "CREATE TEMP TABLE ", weight_stage, " (\n",
      "  lookup_version VARCHAR(128), disease_number INTEGER,\n",
      "  feature_id VARCHAR(128), weight DOUBLE PRECISION\n",
      ") DISTSTYLE ALL SORTKEY(feature_id);"
    )
  )
  weight_insert <- weight_lookup[, c(
    "lookup_version", "disease_number", "feature_id", "weight"
  )]
  weight_insert$disease_number <- as.character(weight_insert$disease_number)
  weight_insert$weight <- as.character(weight_insert$weight)
  insert_data_frame(
    con,
    weight_stage,
    names(weight_insert),
    weight_insert,
    numeric_columns = c("disease_number", "weight")
  )

  missing_weight_qa <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*)::BIGINT AS n_missing\n",
      "FROM (SELECT DISTINCT feature_id FROM ", matches_identifier,
      " WHERE metric = 'CFI') m\n",
      "LEFT JOIN ", weight_stage, " w ON w.feature_id = m.feature_id\n",
      "WHERE w.feature_id IS NULL"
    )
  )
  if (missing_weight_qa$n_missing[[1]] != 0) {
    stop("At least one matched CFI feature has no supplied model weight.")
  }

  phase_started <- start_phase("Calculate CFI scores")
  execute_sql(
    con,
    paste0(
      "CREATE TABLE ", scores_identifier, "\n",
      "DISTKEY(patid) SORTKEY(analysis_year, patid) AS\n",
      "WITH weighted_features AS (\n",
      "  SELECT DISTINCT m.patid, m.analysis_year, w.disease_number,\n",
      "         w.weight, w.lookup_version\n",
      "  FROM ", matches_identifier, " m\n",
      "  JOIN ", weight_stage, " w ON w.feature_id = m.feature_id\n",
      "  WHERE m.metric = 'CFI' AND m.analysis_year IN (", year_list, ")\n",   # CHANGED: was = 2022
      "), weighted_scores AS (\n",
      "  SELECT patid, analysis_year,\n",
      "         COUNT(DISTINCT disease_number)::INTEGER AS cfi_feature_count,\n",
      "         SUM(weight)::DOUBLE PRECISION AS cfi_weight_sum,\n",
      "         MAX(lookup_version) AS lookup_version\n",
      "  FROM weighted_features GROUP BY patid, analysis_year\n",
      ")\n",
      "SELECT ids.patid, ids.patient_id, ids.analysis_year,\n",
      "       (", format(config$cfi_intercept, scientific = FALSE),
      " + COALESCE(s.cfi_weight_sum, 0))::DOUBLE PRECISION AS cfi_score,\n",
      "       COALESCE(s.cfi_feature_count, 0)::INTEGER AS cfi_feature_count,\n",
      "       CASE WHEN COALESCE(s.cfi_feature_count, 0) = 0 THEN 1 ELSE 0 END::INTEGER\n",
      "         AS cfi_intercept_only_flag,\n",
      "       COALESCE(s.cfi_weight_sum, 0)::DOUBLE PRECISION AS cfi_weight_sum,\n",
      "       COALESCE(s.lookup_version, (SELECT MAX(lookup_version) FROM ",
      weight_stage, ")) AS lookup_version\n",
      "FROM ", ids_identifier, " ids\n",
      "LEFT JOIN weighted_scores s\n",
      "  ON s.patid = ids.patid AND s.analysis_year = ids.analysis_year\n",
      "WHERE ids.analysis_year IN (", year_list, ");"   # CHANGED: was = 2022
    )
  )
  finish_phase("Calculate CFI scores", phase_started)

  check_one_row_per_patient_year(con, scores_identifier, "CFI score table")
  qa <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(ids.patid)::BIGINT AS denominator_rows,\n",
      "       COUNT(score.patid)::BIGINT AS score_rows,\n",
      "       SUM(CASE WHEN score.cfi_score IS NULL THEN 1 ELSE 0 END)::BIGINT AS missing_scores,\n",
      "       MIN(score.cfi_score) AS min_score, MAX(score.cfi_score) AS max_score\n",
      "FROM ", ids_identifier, " ids\n",
      "LEFT JOIN ", scores_identifier, " score ON score.patid = ids.patid"
    )
  )
  print(qa)
  if (qa$denominator_rows[[1]] != qa$score_rows[[1]] || qa$missing_scores[[1]] != 0) {
    stop("CFI score table failed denominator coverage checks.")
  }
  invisible(config$cfi_scores_table)
}

run_calculate_cfi()
