# Project: Komodo_generalizability - shareable index-based polypharmacy
# Author: Nemo Zhou
# Date started: 2026-11-09
# Date last updated: 2026-11-09
# Purpose: Stage the selected frozen NDC11-to-ATC4 mapping, derive ATC3,
# clip exposures to each index window, and merge same-class intervals. Mapping
# is loaded from the bundle's mapper output; no online lookups occur here.

pp_sources <- vapply(sys.frames(), function(frame) {
  if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile)[1L]
}, character(1))
pp_script <- tail(pp_sources[!is.na(pp_sources)], 1L)
if (!length(pp_script)) pp_script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
source(file.path(dirname(pp_script), "1_configure_polypharmacy.R"), local = TRUE)

run_build_polypharmacy_exposures <- function() {
  cfg <- polypharmacy_config()
  # Crosswalks contain drug codes and provenance only.
  if (!file.exists(cfg$manifest_path) || !file.exists(cfg$crosswalk_path)) {
    stop("Run 2.5_map_ndc11_to_atc.ps1 first for mapping_version = ", cfg$mapping_version)
  }
  manifest <- jsonlite::fromJSON(cfg$manifest_path)
  if (!identical(manifest$mapping_version, cfg$mapping_version) || !isTRUE(manifest$complete)) {
    stop("Crosswalk manifest does not identify the selected completed mapping.")
  }
  manifest_policy <- if (is.null(manifest$relation_policy)) "n2c_combined" else manifest$relation_policy
  if (!identical(manifest_policy, cfg$relation_policy)) {
    stop("Crosswalk uses another relationship-source policy. Run mapping with a new version.")
  }
  if (!identical(unname(tools::md5sum(cfg$crosswalk_path)), manifest$crosswalk_md5)) {
    stop("Crosswalk differs from its saved mapping manifest.")
  }
  map <- utils::read.csv(cfg$crosswalk_path, colClasses = "character", stringsAsFactors = FALSE)
  if (!all(c("ndc11", "atc4", "mapping_status", "mapping_version") %in% names(map))) stop("Unexpected crosswalk columns.")
  if (anyNA(map$ndc11) || anyNA(map$mapping_version) || anyNA(map$mapping_status) ||
      any(!grepl("^[0-9]{11}$", map$ndc11)) || any(map$mapping_version != cfg$mapping_version)) stop("Invalid crosswalk NDC11/version.")
  mapped <- map$mapping_status == "mapped"
  if (any(!map$mapping_status %in% c("mapped", "unmapped")) ||
      any(is.na(map$atc4[mapped]) | !grepl("^[A-Z][0-9]{2}[A-Z]{2}$", map$atc4[mapped]))) {
    stop("Invalid mapping status or ATC4 format.")
  }
  map$atc3 <- ifelse(mapped, substr(map$atc4, 1L, 4L), NA_character_)
  map <- unique(map[c("ndc11", "atc4", "atc3", "mapping_status", "mapping_version")])
  con <- pp_connect(cfg)
  on.exit(DatabaseConnector::disconnect(con), add = TRUE)
  state <- pp_state(con, cfg)
  pp_require_columns(con, cfg$write_schema, cfg$tables[["indices"]],
                     c("index_id", "patient_id", "window_start", "window_end", "extraction_id", "lookback_months"))
  pp_require_columns(con, cfg$write_schema, cfg$tables[["fills"]],
                     c("index_id", "patient_id", "pharmacy_event_id", "fill_date", "ndc11", "days_supply", "extraction_id"))
  pp_check_outputs(con, cfg, c("crosswalk", "intervals"))
  indices <- pp_table(cfg$write_schema, cfg$tables[["indices"]])
  fills <- pp_table(cfg$write_schema, cfg$tables[["fills"]])
  crosswalk <- pp_table(cfg$write_schema, cfg$tables[["crosswalk"]])
  for (table in c(indices, fills)) {
    if (pp_query(con, pp_lineage_sql(table, state$extraction_id))$n > 0) stop("Input tables belong to different extractions. Rerun stage 2.")
  }
  exposure_id <- pp_new_id()
  pp_update_state(con, cfg, state$extraction_id, exposure_id = exposure_id,
    exposure_status = "BUILDING", metrics_status = "STALE",
    mapping_version = cfg$mapping_version, relation_policy = cfg$relation_policy)
  map$relation_policy <- cfg$relation_policy
  pp_phase(con, "Create selected crosswalk table", paste0("DROP TABLE IF EXISTS ", crosswalk,
    "; CREATE TABLE ", crosswalk, " (ndc11 VARCHAR(11), atc4 VARCHAR(5), atc3 VARCHAR(4),
      mapping_status VARCHAR(16), mapping_version VARCHAR(128), relation_policy VARCHAR(32)) DISTKEY(ndc11) SORTKEY(ndc11);"))
  pp_insert(con, crosswalk, map)
  missing <- pp_query(con, pp_render("SELECT COUNT(*) AS n FROM
    (SELECT DISTINCT ndc11 FROM {fills}) f LEFT JOIN (SELECT DISTINCT ndc11 FROM {crosswalk}) c
    ON f.ndc11 = c.ndc11 WHERE c.ndc11 IS NULL", fills = fills, crosswalk = crosswalk))
  if (missing$n > 0) stop("Selected crosswalk does not cover the current unique NDC input. Use a new mapping version.")
  coverage <- pp_query(con, pp_render("WITH classes AS (
      SELECT ndc11, COUNT(DISTINCT atc3) AS n_classes FROM {crosswalk} GROUP BY ndc11
    ), counts AS (SELECT ndc11, COUNT(*) AS n_index_fills FROM {fills} GROUP BY ndc11)
    SELECT CASE WHEN c.n_classes > 0 THEN 'mapped' ELSE 'unmapped' END AS mapping_status,
      COUNT(*) AS n_unique_ndc11, SUM(f.n_index_fills) AS n_index_fills,
      SUM(CASE WHEN c.n_classes > 1 THEN 1 ELSE 0 END) AS ndcs_with_multiple_atc3
    FROM counts f INNER JOIN classes c ON f.ndc11 = c.ndc11
    GROUP BY CASE WHEN c.n_classes > 0 THEN 'mapped' ELSE 'unmapped' END", fills = fills, crosswalk = crosswalk))
  pp_write_csv(coverage, "3_mapping_summary.csv", cfg)
  print(coverage)
  coverage_by_year <- pp_query(con, pp_render("WITH distinct_fills AS (
      SELECT DISTINCT patient_id, pharmacy_event_id, fill_date, ndc11 FROM {fills}
    ), mapped AS (
      SELECT ndc11, MAX(CASE WHEN mapping_status = 'mapped' THEN 1 ELSE 0 END) AS has_mapping
      FROM {crosswalk} GROUP BY ndc11
    ) SELECT EXTRACT(year FROM f.fill_date)::INTEGER AS fill_year, COUNT(*) AS n_source_fills,
      SUM(CASE WHEN m.has_mapping = 1 THEN 1 ELSE 0 END) AS n_mapped_source_fills,
      SUM(CASE WHEN m.has_mapping = 1 THEN 0 ELSE 1 END) AS n_unmapped_source_fills
    FROM distinct_fills f LEFT JOIN mapped m ON f.ndc11 = m.ndc11
    GROUP BY EXTRACT(year FROM f.fill_date) ORDER BY fill_year", fills = fills, crosswalk = crosswalk))
  coverage_by_year$percent_mapped <- 100 * coverage_by_year$n_mapped_source_fills / coverage_by_year$n_source_fills
  pp_write_csv(coverage_by_year, "3_mapping_by_fill_year.csv", cfg)
  pp_phase(con, "Map fills and clip exposure to index windows", paste0(
    "CREATE TEMP TABLE pp_exposures DISTKEY(index_id) SORTKEY(index_id) AS ",
    pp_exposure_sql(indices, fills, crosswalk)))
  pp_build(con, cfg, "intervals", paste0("SELECT m.*, ", pp_literal(cfg$mapping_version),
    "::VARCHAR(128) AS mapping_version, ", pp_literal(state$extraction_id),
    "::VARCHAR(128) AS extraction_id, ", pp_literal(exposure_id), "::VARCHAR(128) AS exposure_id, ",
    pp_literal(cfg$relation_policy), "::VARCHAR(32) AS relation_policy FROM (", pp_merge_sql("pp_exposures"), ") m"))
  pp_update_state(con, cfg, state$extraction_id, exposure_status = "READY")
  message("Next run 4_calculate_and_validate_polypharmacy.R.")
  invisible(coverage)
}

run_build_polypharmacy_exposures()
