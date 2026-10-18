# 2: Who we measure care quality for.
# Our matched people (ps_matched): each person with HIV in a year, plus their matched control.
# We add age, sex, state and race, because some measures only count certain ages
# (for example, the diabetes measure is only for ages 18-75).
#
# Input:  ps_matched (matching step) + eligible_person_years (HIV cohort step, File 03)
# Output: qm_person_years (one row per person per year)
#         ("qm" = quality measures; every table in this folder starts with qm_)
#
# How to run: open this file in RStudio and click "Source" (top right).

source("1_setup.R")

run_sql("
DROP TABLE IF EXISTS work_wang_life297.qm_person_years;

CREATE TABLE work_wang_life297.qm_person_years AS
SELECT m.pair_id,
       e.patient_id,
       m.analysis_year,
       m.hiv,                  -- 1 = has HIV, 0 = matched control
       e.year_start,           -- Jan 1
       e.index_date,           -- Dec 31
       e.age,
       e.patient_gender,
       e.patient_state,
       e.patient_race_ethnicity
FROM work_wang_life297.ps_matched m
JOIN work_wang_life297.eligible_person_years e
  ON CAST(e.patient_id AS VARCHAR) = m.patient_id
 AND e.analysis_year = m.analysis_year;
")

# check: should have the same number of rows as ps_matched
show("
SELECT (SELECT COUNT(*) FROM work_wang_life297.ps_matched)      AS ps_matched_rows,
       (SELECT COUNT(*) FROM work_wang_life297.qm_person_years) AS qm_person_years_rows;
")

# check: each year should have the same number of people with and without HIV
show("
SELECT analysis_year, SUM(hiv) AS with_hiv, SUM(1 - hiv) AS without_hiv
FROM work_wang_life297.qm_person_years
GROUP BY 1 ORDER BY 1;
")

disconnect(con)
