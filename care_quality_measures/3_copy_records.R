# 3: Copy our people's records into 3 small tables.
# Komodo's tables are huge. Instead of searching them again and again, we copy only
# OUR people's records once. Every later file searches these small tables, so it's fast.
#   qm_dx_events        = diagnoses
#   qm_procedure_events = tests, exams and procedures
#   qm_rx_events        = drug fills
# Years: 2015-2025. We keep 2015 because the diabetes measure also looks at the year before.
#
# This is the only slow file (it may take a few hours). The rest take minutes.
# Bonus: Nemo's files 3 and 6 can also use qm_dx_events and qm_procedure_events
# (see the speed-up guide), so the big tables are only read once in total.
#
# Input:  qm_person_years (File 2)
# Output: qm_dx_events, qm_procedure_events, qm_rx_events
#
# How to run: click "Source".

source("1_setup.R")

run_sql("
DROP TABLE IF EXISTS work_wang_life297.qm_dx_events;
CREATE TABLE work_wang_life297.qm_dx_events AS
SELECT patient_id, event_date, dx_code, source_table
FROM komodo_202606.normalized_dx_events
WHERE event_date BETWEEN '2015-01-01' AND '2025-12-31'
  AND patient_id IN (SELECT patient_id FROM work_wang_life297.qm_person_years);

DROP TABLE IF EXISTS work_wang_life297.qm_procedure_events;
CREATE TABLE work_wang_life297.qm_procedure_events AS
SELECT patient_id, event_date, procedure_code
FROM komodo_202606.normalized_procedure_events
WHERE event_date BETWEEN '2015-01-01' AND '2025-12-31'
  AND patient_id IN (SELECT patient_id FROM work_wang_life297.qm_person_years);

DROP TABLE IF EXISTS work_wang_life297.qm_rx_events;
CREATE TABLE work_wang_life297.qm_rx_events AS
SELECT patient_id, fill_date, generic_name, brand_name
FROM komodo_202606.pharmacy_events
WHERE fill_date BETWEEN '2015-01-01' AND '2025-12-31'
  AND patient_id IN (SELECT patient_id FROM work_wang_life297.qm_person_years);
")

# check 1: each small table should have rows
show("
SELECT 'diagnoses' AS what, COUNT(*) AS records, COUNT(DISTINCT patient_id) AS people FROM work_wang_life297.qm_dx_events
UNION ALL
SELECT 'procedures', COUNT(*), COUNT(DISTINCT patient_id) FROM work_wang_life297.qm_procedure_events
UNION ALL
SELECT 'drug fills', COUNT(*), COUNT(DISTINCT patient_id) FROM work_wang_life297.qm_rx_events;
")

# check 2: do the test codes we need show up in the data? (send a screenshot)
#   Insurance claims don't include lab results, so some measures rely on these codes.
#   83036, 83037 = HbA1c test        3044F-3052F = HbA1c result
#   3074F-3080F  = blood pressure result
#   99483 = memory check (dementia)   1007F = pain medicine check (osteoarthritis)
#   2028F = foot exam (diabetes)      92551, 92651-92653 = hearing test
#   99172, 99173 = vision screening   43235-43259 = upper GI scope (GERD)
show("
SELECT procedure_code, COUNT(DISTINCT patient_id) AS people
FROM work_wang_life297.qm_procedure_events
WHERE procedure_code IN ('83036','83037','3044F','3046F','3051F','3052F',
                         '3074F','3075F','3077F','3078F','3079F','3080F',
                         '99483','1007F','2028F','92551','92651','92652','92653','99172','99173')
   OR procedure_code BETWEEN '43235' AND '43259'
GROUP BY 1 ORDER BY 1;
")

# check 3: how are the drugs we need written in the data? (send a screenshot)
#   stomach acid drugs (-prazole), heart and blood pressure drugs (-pril, -sartan),
#   COPD inhalers (tiotropium etc.), prednisone
show("
SELECT generic_name, COUNT(DISTINCT patient_id) AS people
FROM work_wang_life297.qm_rx_events
WHERE generic_name ~* 'prazole|pril|sartan|tiotropium|umeclidinium|aclidinium|glycopyrr|prednisone'
GROUP BY 1 ORDER BY 2 DESC LIMIT 100;
")

disconnect(con)
