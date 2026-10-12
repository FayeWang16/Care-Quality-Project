source("Code/shareable_clinical_metrics/1_configure_clinical_metrics.R")

# Project: Care Quality (multimorbidity care quality in people with and without HIV), 2016-2025
# Adapted from Nemo Zhou's Frailty_Komoto shareable clinical metrics code
# Author: Lifei Wang
# Acknowledgements: Nemo Zhou
# Date started: 2026-10-12
# Date last updated: 2026-10-12
#
# ---- Purpose ----
# Calculate Gagne's combined comorbidity score from the compact diagnosis
# feature matches. Each supplied Gagne group contributes at most once per
# patient-year. The output retains every group indicator, the number of observed
# groups, and the sum of their supplied weights. Patient-years without a matched
# group receive zero indicators, a zero group count, and a zero score.
#
# ---- Adapted for the Care Quality project ----
# CHANGED: scores every year in config$analysis_year (2016-2025), not only 2022.

gagne_indicator_name <- function(group_number, description) {
  cleaned <- tolower(description)
  cleaned <- gsub("^gagne_", "", cleaned)
  cleaned <- gsub("[^a-z0-9]+", "_", cleaned)
  cleaned <- gsub("^_+|_+$", "", cleaned)
  paste0("gagne_group_", sprintf("%02d", group_number), "_", cleaned)
}

run_calculate_gagne <- function() {
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
    config$gagne_group_matches_table,
    c("patid", "analysis_year", "metric", "feature_id", "lookup_version")
  )

  diagnosis_lookup <- read_lookup_csv(file.path(
    config$lookup_dir,
    "0.6_gagne_diagnosis_lookup.csv"
  ))
  weight_lookup <- read_lookup_csv(file.path(
    config$lookup_dir,
    "0.6_gagne_weight_lookup.csv"
  ))
  require_columns(
    diagnosis_lookup,
    c("feature_id", "gagne_group", "gagne_group_desc"),
    "Gagne diagnosis lookup"
  )
  require_columns(
    weight_lookup,
    c("lookup_version", "gagne_group", "gagne_group_desc", "weight"),
    "Gagne weight lookup"
  )

  groups <- unique(weight_lookup[, c(
    "lookup_version", "gagne_group", "gagne_group_desc", "weight"
  )])
  groups$gagne_group <- as.integer(groups$gagne_group)
  groups$weight <- as.numeric(groups$weight)
  groups <- groups[
    !is.na(groups$gagne_group) & !is.na(groups$weight) &
      !is.na(groups$gagne_group_desc) & nzchar(groups$gagne_group_desc),
  ]
  groups <- groups[order(groups$gagne_group), ]
  if (nrow(groups) == 0L || anyDuplicated(groups$gagne_group)) {
    stop("Gagne weight lookup must contain one usable row per group.")
  }
  groups$indicator_column <- mapply(
    gagne_indicator_name,
    groups$gagne_group,
    groups$gagne_group_desc,
    USE.NAMES = FALSE
  )
  if (anyDuplicated(groups$indicator_column)) {
    stop("Derived Gagne indicator column names are not unique.")
  }

  feature_groups <- unique(diagnosis_lookup[, c("feature_id", "gagne_group")])
  feature_groups$gagne_group <- as.integer(feature_groups$gagne_group)
  feature_groups <- merge(
    feature_groups,
    groups[, c(
      "lookup_version", "gagne_group", "gagne_group_desc", "weight"
    )],
    by = "gagne_group",
    all.x = TRUE
  )
  if (any(is.na(feature_groups$weight))) {
    stop("At least one Gagne diagnosis feature maps to a group without a weight.")
  }

  prepare_output_table(con, config, config$gagne_scores_table)
  ids_identifier <- qualified_identifier(config$write_schema, config$metric_ids_table)
  matches_identifier <- qualified_identifier(
    config$write_schema,
    config$gagne_group_matches_table
  )
  scores_identifier <- qualified_identifier(config$write_schema, config$gagne_scores_table)
  group_stage <- quote_identifier("share_gagne_feature_group_stage")

  execute_sql(
    con,
    paste0(
      "CREATE TEMP TABLE ", group_stage, " (\n",
      "  lookup_version VARCHAR(128), feature_id VARCHAR(128),\n",
      "  gagne_group INTEGER, gagne_group_desc VARCHAR(256),\n",
      "  weight DOUBLE PRECISION\n",
      ") DISTSTYLE ALL SORTKEY(feature_id);"
    )
  )
  feature_groups$gagne_group <- as.character(feature_groups$gagne_group)
  feature_groups$weight <- as.character(feature_groups$weight)
  insert_data_frame(
    con,
    group_stage,
    c("lookup_version", "feature_id", "gagne_group", "gagne_group_desc", "weight"),
    feature_groups,
    numeric_columns = c("gagne_group", "weight")
  )

  missing_weight_qa <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*)::BIGINT AS n_missing\n",
      "FROM (SELECT DISTINCT feature_id FROM ", matches_identifier,
      " WHERE metric = 'GAGNE') m\n",
      "LEFT JOIN ", group_stage, " g ON g.feature_id = m.feature_id\n",
      "WHERE g.feature_id IS NULL"
    )
  )
  if (missing_weight_qa$n_missing[[1]] != 0) {
    stop("At least one matched Gagne feature has no supplied group weight.")
  }

  indicator_sql <- paste(
    paste0(
      "COALESCE(MAX(CASE WHEN observed.gagne_group = ", groups$gagne_group,
      " THEN 1 ELSE 0 END), 0)::INTEGER AS ",
      quote_identifier(groups$indicator_column)
    ),
    collapse = ",\n       "
  )

  year_list <- paste(config$analysis_year, collapse = ", ")   # CHANGED: e.g. "2016, 2017, ..., 2025"
  phase_started <- start_phase("Calculate Gagne combined comorbidity scores")
  execute_sql(
    con,
    paste0(
      "CREATE TABLE ", scores_identifier, "\n",
      "DISTKEY(patid) SORTKEY(analysis_year, patid) AS\n",
      "WITH observed AS (\n",
      "  SELECT DISTINCT m.patid, m.analysis_year, g.gagne_group,\n",
      "         g.weight, g.lookup_version\n",
      "  FROM ", matches_identifier, " m\n",
      "  JOIN ", group_stage, " g ON g.feature_id = m.feature_id\n",
      "  WHERE m.metric = 'GAGNE' AND m.analysis_year IN (", year_list, ")\n",   # CHANGED: was = 2022
      "), weighted_scores AS (\n",
      "  SELECT patid, analysis_year,\n",
      "         COUNT(DISTINCT gagne_group)::INTEGER AS gagne_group_count,\n",
      "         SUM(weight)::DOUBLE PRECISION AS gagne_score,\n",
      "         MAX(lookup_version) AS lookup_version\n",
      "  FROM observed GROUP BY patid, analysis_year\n",
      ")\n",
      "SELECT ids.patid, ids.patient_id, ids.analysis_year,\n",
      "       ", indicator_sql, ",\n",
      "       COALESCE(score.gagne_group_count, 0)::INTEGER AS gagne_group_count,\n",
      "       COALESCE(score.gagne_score, 0)::DOUBLE PRECISION AS gagne_score,\n",
      "       COALESCE(score.lookup_version, (SELECT MAX(lookup_version) FROM ",
      group_stage, ")) AS lookup_version\n",
      "FROM ", ids_identifier, " ids\n",
      "LEFT JOIN observed\n",
      "  ON observed.patid = ids.patid AND observed.analysis_year = ids.analysis_year\n",
      "LEFT JOIN weighted_scores score\n",
      "  ON score.patid = ids.patid AND score.analysis_year = ids.analysis_year\n",
      "WHERE ids.analysis_year IN (", year_list, ")\n",   # CHANGED: was = 2022
      "GROUP BY ids.patid, ids.patient_id, ids.analysis_year,\n",
      "         score.gagne_group_count, score.gagne_score, score.lookup_version;"
    )
  )
  finish_phase("Calculate Gagne combined comorbidity scores", phase_started)

  check_one_row_per_patient_year(con, scores_identifier, "Gagne score table")
  qa <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(ids.patid)::BIGINT AS denominator_rows,\n",
      "       COUNT(score.patid)::BIGINT AS score_rows,\n",
      "       MIN(score.gagne_score) AS min_score, MAX(score.gagne_score) AS max_score\n",
      "FROM ", ids_identifier, " ids\n",
      "LEFT JOIN ", scores_identifier, " score ON score.patid = ids.patid"
    )
  )
  print(qa)
  if (qa$denominator_rows[[1]] != qa$score_rows[[1]]) {
    stop("Gagne score table failed denominator coverage checks.")
  }
  invisible(config$gagne_scores_table)
}

run_calculate_gagne()
