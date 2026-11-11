# 8 finds people with polypharmacy, each year, among the matched people (Next steps #3).
# Polypharmacy = at least 5 drug classes (ATC3) in use at the same time, on at least 90 days
# of the year (protocol: "5+ ATC-3 classes"; Nemo's code makes this exact).
# Uses Nemo's polypharmacy code as it is (his 6 files, in the folder SHARP-main/polypharmacy).
# Only looks at matched people (File 4, ps_matched), one row per person per year 2016-2025.
# Output: results/full/8_polypharmacy/polypharmacy_by_year.csv (% with polypharmacy,
#         by year and HIV group) and polypharmacy_plot.pdf
# Run from the SHARP-main folder. Run it once, from top to bottom.
# Step 2 looks up each drug online and can take hours; if it stops, run the file again
# and it carries on where it stopped.

# ---- settings ----
options(komodo.shareable_polypharmacy.config = list(
  study_root        = getwd(),
  komodo_schema     = "komodo_202610",       # Komodo version for the pharmacy claims
  cohort_table      = "cq_poly_cohort",      # our list of who and which year (made in step 1)
  index_id_column   = "index_id",
  patient_id_column = "patient_id",
  index_date_column = "index_date",
  table_prefix      = "cq_poly12m_",         # names of Nemo's tables for this project
  mapping_version   = "care_quality_atcprod_20261110",   # a label for our drug-class lookup
  relation_policy   = "atcprod_only",
  replace_existing  = TRUE                   # a rerun rebuilds the tables instead of stopping
))
source("polypharmacy/1_configure_polypharmacy.R")      # Nemo's helpers (connect, query, ...)
cfg <- polypharmacy_config()
con <- pp_connect(cfg)
WORK <- cfg$write_schema                                # our work schema, work_wang_life297

# ---- step 1: the list of who and which year ----
# One row per matched person per year, from File 4 (ps_matched).
# Nemo's code looks at the 12 months BEFORE index_date, so index_date is Jan 1 of the NEXT year:
#   2020 -> index_date 2021-01-01 -> looks at 2020-01-01 to 2020-12-31
pp_exec(con, paste0("
DROP TABLE IF EXISTS ", WORK, ".cq_poly_cohort;

CREATE TABLE ", WORK, ".cq_poly_cohort AS
SELECT DISTINCT
       CAST(patient_id AS VARCHAR(256)) || '_' || CAST(analysis_year AS VARCHAR(4)) AS index_id,
       CAST(patient_id AS VARCHAR(256)) AS patient_id,
       analysis_year,
       hiv,                                                        -- 1 = HIV, 0 = matched control
       TO_DATE(CAST(analysis_year + 1 AS VARCHAR(4)) || '-01-01', 'YYYY-MM-DD') AS index_date
FROM ", WORK, ".ps_matched;"))

# check: one row per person-year (the two numbers should match)
print(pp_query(con, paste0("SELECT COUNT(*) AS rows, COUNT(DISTINCT index_id) AS person_years
                            FROM ", WORK, ".cq_poly_cohort")))

# ---- step 2: Nemo's code ----
# 2a. finds each person's paid pharmacy fills in the year, and lists the drug codes (NDC11)
source("polypharmacy/2_prepare_polypharmacy_inputs.R")

# 2b. looks up each drug code's class online (NLM RxNav). This is the slow part: it prints
#     its progress every 100 codes. If it stops, run this file again: it reuses what it found.
status <- system2("python", c("polypharmacy/n2c_mapper.py",
                              "--input", shQuote(cfg$ndc_path),
                              "--output-dir", shQuote(cfg$mapping_dir),
                              "--mapping-version", cfg$mapping_version,
                              "--relation-policy", cfg$relation_policy))
if (status != 0) stop("The drug-class lookup stopped. Run this file again to carry on.")

# 2c. works out which drug classes each person was on, day by day
source("polypharmacy/3_build_polypharmacy_exposures.R")

# 2d. gives each person-year polypharmacy = 1 (5+ classes at once on 90+ days) or 0
source("polypharmacy/4_calculate_and_validate_polypharmacy.R")

# ---- step 3: % with polypharmacy, by year and HIV group ----
by_year <- pp_query(con, paste0("
SELECT c.analysis_year,
       c.hiv,
       COUNT(*)                               AS person_years,       -- people in the group
       SUM(m.polypharmacy)                    AS n_polypharmacy,     -- of those, with polypharmacy
       100.0 * SUM(m.polypharmacy) / COUNT(*) AS pct_polypharmacy
FROM ", WORK, ".cq_poly12m_metrics m                                -- Nemo's result (step 2d)
JOIN ", WORK, ".cq_poly_cohort     c ON c.index_id = m.index_id    -- our list (step 1)
GROUP BY 1, 2
ORDER BY 1, 2"))
DatabaseConnector::disconnect(con)

# ---- step 4: save ----
# small-cell rule (as in every other phase): counts of 1 to 10 are shown as "<11", and that
# row's % is left blank, so nobody can be picked out
saved <- by_year
small <- saved$person_years %in% 1:10 | saved$n_polypharmacy %in% 1:10
saved$pct_polypharmacy <- ifelse(small, NA, round(saved$pct_polypharmacy, 1))
saved$person_years     <- ifelse(saved$person_years   %in% 1:10, "<11", saved$person_years)
saved$n_polypharmacy   <- ifelse(saved$n_polypharmacy %in% 1:10, "<11", saved$n_polypharmacy)

out_dir <- "results/full/8_polypharmacy"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(saved, file.path(out_dir, "polypharmacy_by_year.csv"), row.names = FALSE, na = "")
print(saved)

# a picture for the slides: one line per group
pdf(file.path(out_dir, "polypharmacy_plot.pdf"), width = 8, height = 5, compress = FALSE)
ctl <- by_year[by_year$hiv == 0, ]
pwh <- by_year[by_year$hiv == 1, ]
plot(by_year$analysis_year, by_year$pct_polypharmacy, type = "n",
     xlab = "Year", ylab = "% with polypharmacy", main = "Polypharmacy by year")
lines(ctl$analysis_year, ctl$pct_polypharmacy, type = "b", pch = 16, col = "steelblue", lty = 2)
lines(pwh$analysis_year, pwh$pct_polypharmacy, type = "b", pch = 17, col = "darkorange")
legend("topleft", c("Matched controls", "People with HIV"),
       col = c("steelblue", "darkorange"), pch = c(16, 17), lty = c(2, 1), bty = "n")
dev.off()
