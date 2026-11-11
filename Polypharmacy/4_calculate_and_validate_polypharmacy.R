# Project: Komodo_generalizability - shareable index-based polypharmacy
# Author: Nemo Zhou
# Date started: 2026-11-09
# Date last updated: 2026-11-09
# Purpose: Count concurrent ATC3 classes with interval change events, calculate
# days above the class threshold, and retain every input index episode in a
# final Redshift table. Export aggregate summaries, not patient-level results.

pp_sources <- vapply(sys.frames(), function(frame) {
  if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile)[1L]
}, character(1))
pp_script <- tail(pp_sources[!is.na(pp_sources)], 1L)
if (!length(pp_script)) pp_script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
source(file.path(dirname(pp_script), "1_configure_polypharmacy.R"), local = TRUE)

run_calculate_polypharmacy <- function() {
  cfg <- polypharmacy_config()
  con <- pp_connect(cfg)
  on.exit(DatabaseConnector::disconnect(con), add = TRUE)
  state <- pp_state(con, cfg, exposures = TRUE)
  for (key in c("indices", "fills", "crosswalk", "intervals")) {
    pp_query(con, paste0("SELECT * FROM ", pp_table(cfg$write_schema, cfg$tables[[key]]), " LIMIT 0"))
  }
  pp_check_outputs(con, cfg, "metrics")
  indices <- pp_table(cfg$write_schema, cfg$tables[["indices"]])
  fills <- pp_table(cfg$write_schema, cfg$tables[["fills"]])
  crosswalk <- pp_table(cfg$write_schema, cfg$tables[["crosswalk"]])
  intervals <- pp_table(cfg$write_schema, cfg$tables[["intervals"]])
  final <- pp_table(cfg$write_schema, cfg$tables[["metrics"]])
  for (table in c(indices, fills)) {
    if (pp_query(con, pp_lineage_sql(table, state$extraction_id))$n > 0) stop("Input extraction IDs disagree. Rerun stage 2.")
  }
  if (pp_query(con, pp_lineage_sql(crosswalk, extraction_id = NULL,
      mapping_version = cfg$mapping_version, relation_policy = cfg$relation_policy))$n > 0) {
    stop("Crosswalk version or source policy differs. Rerun stage 3.")
  }
  if (pp_query(con, pp_lineage_sql(intervals, state$extraction_id, state$exposure_id,
      cfg$mapping_version, cfg$relation_policy))$n > 0) stop("Stale exposure intervals. Rerun stage 3.")
  invalid <- pp_query(con, pp_render("SELECT COUNT(*) AS n FROM {intervals} m
    LEFT JOIN {indices} i ON m.index_id = i.index_id
    WHERE i.index_id IS NULL OR m.interval_start IS NULL OR m.interval_end IS NULL
      OR m.interval_start < i.window_start OR m.interval_end > i.window_end
      OR m.interval_end < m.interval_start", intervals = intervals, indices = indices))
  if (invalid$n > 0) stop("Exposure intervals fall outside their index windows.")
  pp_phase(con, "Calculate concurrent ATC3 exposure spans", paste0(
    "CREATE TEMP TABLE pp_active_spans AS ", pp_spans_sql(intervals)))
  negative <- pp_query(con, "SELECT COUNT(*) AS n FROM pp_active_spans WHERE active_classes < 0")
  if (negative$n > 0) stop("Negative active-class count.")
  pp_update_state(con, cfg, state$extraction_id, metrics_status = "BUILDING")
  sql <- pp_render("WITH duration AS (
    SELECT index_id,
      SUM(CASE WHEN active_classes >= {classes} THEN span_days ELSE 0 END) AS days_5plus_atc3,
      SUM(CASE WHEN active_classes > 0 THEN span_days ELSE 0 END) AS days_any_atc3,
      MAX(active_classes) AS maximum_concurrent_atc3
    FROM pp_active_spans GROUP BY index_id
  ), class_counts AS (
    SELECT index_id, COUNT(DISTINCT atc3) AS n_distinct_atc3 FROM {intervals} GROUP BY index_id
  ), ndc_status AS (
    SELECT ndc11, MAX(CASE WHEN mapping_status = 'mapped' THEN 1 ELSE 0 END) AS mapped
    FROM {crosswalk} GROUP BY ndc11
  ), fill_counts AS (
    SELECT f.index_id, COUNT(*) AS n_fills,
      SUM(CASE WHEN c.mapped = 1 THEN 1 ELSE 0 END) AS n_mapped_fills,
      SUM(CASE WHEN c.mapped = 1 THEN 0 ELSE 1 END) AS n_unmapped_fills
    FROM {fills} f LEFT JOIN ndc_status c ON f.ndc11 = c.ndc11 GROUP BY f.index_id
  ) SELECT i.*, DATEDIFF(day, i.window_start, i.index_date)::INTEGER AS window_days,
    COALESCE(d.days_5plus_atc3, 0)::INTEGER AS days_5plus_atc3,
    CASE WHEN COALESCE(d.days_5plus_atc3, 0) >= {days} THEN 1 ELSE 0 END::INTEGER AS polypharmacy,
    COALESCE(d.days_any_atc3, 0)::INTEGER AS days_any_atc3,
    COALESCE(d.maximum_concurrent_atc3, 0)::INTEGER AS maximum_concurrent_atc3,
    COALESCE(c.n_distinct_atc3, 0)::INTEGER AS n_distinct_atc3,
    COALESCE(f.n_fills, 0)::BIGINT AS n_fills,
    COALESCE(f.n_mapped_fills, 0)::BIGINT AS n_mapped_fills,
    COALESCE(f.n_unmapped_fills, 0)::BIGINT AS n_unmapped_fills,
    {classes}::INTEGER AS minimum_classes,
    {days}::INTEGER AS minimum_days, 'index_polypharmacy_2'::VARCHAR(64) AS definition_version,
    'n2c_rxnav_atc4_to_atc3'::VARCHAR(64) AS mapping_source,
    {version}::VARCHAR(128) AS mapping_version,
    {exposure}::VARCHAR(128) AS exposure_id, {policy}::VARCHAR(32) AS relation_policy
    FROM {indices} i LEFT JOIN duration d ON i.index_id = d.index_id
    LEFT JOIN class_counts c ON i.index_id = c.index_id
    LEFT JOIN fill_counts f ON i.index_id = f.index_id",
    classes = cfg$minimum_classes, days = cfg$minimum_days,
    intervals = intervals, crosswalk = crosswalk, fills = fills, indices = indices,
    version = pp_literal(cfg$mapping_version), exposure = pp_literal(state$exposure_id),
    policy = pp_literal(cfg$relation_policy))
  pp_build(con, cfg, "metrics", sql)
  integrity <- pp_query(con, pp_render("SELECT COUNT(*) AS n_rows, COUNT(DISTINCT index_id) AS n_ids,
    (SELECT COUNT(*) FROM {indices}) AS n_input,
    SUM(CASE WHEN days_5plus_atc3 < 0 OR days_5plus_atc3 > window_days
      OR polypharmacy <> CASE WHEN days_5plus_atc3 >= minimum_days THEN 1 ELSE 0 END
      OR n_mapped_fills + n_unmapped_fills <> n_fills THEN 1 ELSE 0 END) AS invalid
    FROM {final}", indices = indices, final = final))
  if (integrity$n_rows != integrity$n_ids || integrity$n_rows != integrity$n_input ||
      (!is.na(integrity$invalid) && integrity$invalid > 0)) stop("Final index-level results failed consistency checks.")
  summary <- pp_query(con, paste0("SELECT COUNT(*) AS n_index_episodes,
      COUNT(DISTINCT patient_id) AS n_patients, COALESCE(SUM(polypharmacy), 0) AS n_polypharmacy,
      100.0 * SUM(polypharmacy) / NULLIF(COUNT(*), 0) AS percent_polypharmacy,
      COALESCE(SUM(CASE WHEN n_fills = 0 THEN 1 ELSE 0 END), 0) AS n_indices_without_fills,
      COALESCE(SUM(CASE WHEN n_unmapped_fills > 0 THEN 1 ELSE 0 END), 0) AS n_indices_with_unmapped_fills,
      AVG(days_5plus_atc3::DOUBLE PRECISION) AS mean_days_5plus_atc3
    FROM ", final))
  pp_write_csv(summary, "4_polypharmacy_summary.csv", cfg)
  pp_write_csv(pp_query(con, paste0("SELECT days_5plus_atc3, COUNT(*) AS n_index_episodes FROM ",
    final, " GROUP BY days_5plus_atc3 ORDER BY days_5plus_atc3")), "4_qualifying_days_distribution.csv", cfg)
  print(summary)
  pp_update_state(con, cfg, state$extraction_id, metrics_status = "READY")
  message("Final index-level table: ", final)
  invisible(summary)
}

run_calculate_polypharmacy()
