# Project: Komodo_generalizability - shareable index-based polypharmacy
# Author: Nemo Zhou
# Date started: 2026-11-09
# Date last updated: 2026-11-09
# Purpose: Materialize the supplied index cohort, derive pre-index windows,
# extract paid pharmacy exposure including carry-in, and export unique NDC11.
# Reuse the saved fills for subsequent mapping and scoring stages.

pp_sources <- vapply(sys.frames(), function(frame) {
  if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile)[1L]
}, character(1))
pp_script <- tail(pp_sources[!is.na(pp_sources)], 1L)
if (!length(pp_script)) pp_script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
source(file.path(dirname(pp_script), "1_configure_polypharmacy.R"), local = TRUE)

run_prepare_polypharmacy_inputs <- function() {
  cfg <- polypharmacy_config()
  con <- pp_connect(cfg)
  on.exit(DatabaseConnector::disconnect(con), add = TRUE)
  pp_require_columns(con, cfg$cohort_schema, cfg$cohort_table,
    c(cfg$index_id_column, cfg$patient_id_column, cfg$index_date_column))
  pp_require_columns(con, cfg$komodo_schema, "pharmacy_events",
    c("patient_id", "pharmacy_event_id", "fill_date", "ndc11", "days_supply",
      "transaction_result", "transaction_status", "transaction_source_type"))
  pp_check_outputs(con, cfg, c("indices", "fills", "state"))
  cohort <- pp_table(cfg$cohort_schema, cfg$cohort_table)
  id <- pp_ident(cfg$index_id_column)
  patient <- pp_ident(cfg$patient_id_column)
  date <- pp_ident(cfg$index_date_column)
  input <- pp_query(con, pp_render("SELECT COUNT(*) AS n,
    COUNT(DISTINCT {id}) AS n_ids,
    SUM(CASE WHEN {id} IS NULL OR TRIM(CAST({id} AS VARCHAR(65535))) = ''
      OR {patient} IS NULL OR TRIM(CAST({patient} AS VARCHAR(65535))) = ''
      OR {date} IS NULL OR TRY_CAST({date} AS DATE) IS NULL
      OR LENGTH(CAST({id} AS VARCHAR(65535))) > 256
      OR LENGTH(CAST({patient} AS VARCHAR(65535))) > 256 THEN 1 ELSE 0 END) AS invalid
    FROM {cohort}", id = id, patient = patient, date = date, cohort = cohort))
  if (input$n != input$n_ids || (!is.na(input$invalid) && input$invalid > 0)) {
    stop("Cohort must have unique nonmissing index_id and valid patient_id/index_date.")
  }
  extraction_id <- pp_new_id()
  pp_begin_extraction(con, cfg, extraction_id)

  # Build a lazy cohort and render its SELECT before physical materialization.
  base <- pp_render("SELECT CAST({id} AS VARCHAR(256)) AS index_id,
      CAST({patient} AS VARCHAR(256)) AS patient_id, CAST({date} AS DATE) AS index_date
    FROM {cohort}", id = id, patient = patient, date = date, cohort = cohort)
  windows <- dplyr::tbl(con, dbplyr::sql(base)) |>
    dplyr::mutate(
      window_start = dbplyr::sql(paste0("CAST(DATEADD(month, -", cfg$lookback_months, ", index_date) AS DATE)")),
      window_end = dbplyr::sql("CAST(DATEADD(day, -1, index_date) AS DATE)"),
      lookback_months = dbplyr::sql(paste0(cfg$lookback_months, "::INTEGER")),
      extraction_id = dbplyr::sql(paste0(pp_literal(extraction_id), "::VARCHAR(128)")))
  pp_build(con, cfg, "indices", as.character(dbplyr::sql_render(windows)))
  indices <- pp_table(cfg$write_schema, cfg$tables[["indices"]])
  fills <- pp_table(cfg$write_schema, cfg$tables[["fills"]])
  rx <- pp_table(cfg$komodo_schema, "pharmacy_events")

  # Scan each patient's pharmacy history once, not once per index episode.
  # No lower fill-date cutoff: earlier supply can overlap the first window.
  pp_phase(con, "Scan pharmacy history for cohort patients", pp_render("
    CREATE TEMP TABLE pp_rx_candidates DISTKEY(patient_id) SORTKEY(patient_id, fill_date) AS
    WITH bounds AS (SELECT patient_id, MIN(window_start) AS first_start, MAX(window_end) AS last_end
      FROM {indices} GROUP BY patient_id)
    SELECT r.patient_id, r.pharmacy_event_id, CAST(r.fill_date AS DATE) AS fill_date,
      TRIM(r.ndc11) AS ndc11, r.days_supply,
      UPPER(TRIM(r.transaction_result)) AS transaction_result,
      UPPER(TRIM(r.transaction_status)) AS transaction_status,
      r.transaction_source_type
    FROM {rx} r INNER JOIN bounds b ON r.patient_id = b.patient_id
    WHERE r.fill_date <= b.last_end
      AND (r.days_supply IS NULL OR r.days_supply <= 0
        OR DATEADD(day, r.days_supply - 1, r.fill_date) >= b.first_start)", indices = indices, rx = rx))
  source_summary <- pp_query(con, "SELECT transaction_result, transaction_status, transaction_source_type,
      COUNT(*) AS candidate_rows,
      SUM(CASE WHEN ndc11 IS NULL OR ndc11 !~ '^[0-9]{11}$' THEN 1 ELSE 0 END) AS invalid_ndc_rows,
      SUM(CASE WHEN days_supply IS NULL OR days_supply <= 0 THEN 1 ELSE 0 END) AS invalid_supply_rows,
      MIN(days_supply) AS min_days_supply, MAX(days_supply) AS max_days_supply
    FROM pp_rx_candidates GROUP BY transaction_result, transaction_status, transaction_source_type")
  pp_write_csv(source_summary, "2_fill_extraction_summary.csv", cfg)
  print(source_summary)
  pp_phase(con, "Keep valid paid terminal pharmacy events", "CREATE TEMP TABLE pp_rx_valid AS
    SELECT DISTINCT patient_id, pharmacy_event_id, fill_date, ndc11, days_supply
    FROM pp_rx_candidates WHERE transaction_result = 'PAID'
      AND transaction_status IN ('FINAL', 'STANDALONE')
      AND ndc11 ~ '^[0-9]{11}$' AND days_supply > 0")
  conflicts <- pp_query(con, "SELECT COUNT(*) AS n FROM (
      SELECT patient_id, pharmacy_event_id FROM pp_rx_valid
      GROUP BY patient_id, pharmacy_event_id HAVING COUNT(*) > 1 OR pharmacy_event_id IS NULL
    ) conflicts")
  if (conflicts$n > 0) stop("Conflicting or missing patient/event keys in retained pharmacy fills.")
  pp_build(con, cfg, "fills", pp_render("SELECT i.index_id, i.extraction_id, r.patient_id, r.pharmacy_event_id,
      r.fill_date, r.ndc11, r.days_supply
    FROM {indices} i INNER JOIN pp_rx_valid r ON i.patient_id = r.patient_id
      AND r.fill_date <= i.window_end
      AND DATEADD(day, r.days_supply - 1, r.fill_date) >= i.window_start", indices = indices))
  # Only drug codes, never patient/event identifiers, leave Redshift here.
  ndcs <- pp_query(con, paste0("SELECT DISTINCT ndc11 FROM ", fills, " ORDER BY ndc11"))
  pp_archive(cfg$ndc_path, cfg)
  dir.create(cfg$output_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(as.character(ndcs$ndc11), cfg$ndc_path, useBytes = TRUE)
  summary <- pp_query(con, pp_render("SELECT COUNT(*) AS index_fill_rows,
      COUNT(DISTINCT f.index_id) AS indices_with_fills,
      SUM(CASE WHEN f.fill_date < i.window_start THEN 1 ELSE 0 END) AS carry_in_index_fills,
      COUNT(DISTINCT f.ndc11) AS unique_ndc11
    FROM {fills} f INNER JOIN {indices} i ON f.index_id = i.index_id", fills = fills, indices = indices))
  pp_write_csv(summary, "2_input_summary.csv", cfg)
  print(summary)
  pp_update_state(con, cfg, extraction_id, input_status = "READY")
  message("Unique NDC11 input: ", cfg$ndc_path,
          "\nNext run 2.5_map_ndc11_to_atc.ps1, then scripts 3 and 4.")
  invisible(summary)
}

run_prepare_polypharmacy_inputs()
