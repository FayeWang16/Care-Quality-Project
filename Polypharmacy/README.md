# Polypharmacy during the 12 months before an index date

Project: Komodo_generalizability (Frailty_Komoto)  
Author: Nemo Zhou  
Date started: 2026-11-09  
Date last updated: 2026-11-09

## Purpose

These scripts use Komodo pharmacy dispensing claims to calculate polypharmacy
during the 12 months before each supplied index date. Supply an existing
eligible cohort; each analysis unit receives its own window and result.

## Definition

This is the project's operational definition, adapted from the annual SAS
approach described in the [project reference](../../../Documents/09_POLYPHARMACY_REFERENCE.md).
The pre-index window, carry-in rule, and inclusion of antiretroviral therapy
are project-specific choices. NLM references describe mapping resources,
not validation of the five-class/90-day threshold.

**Polypharmacy is at least five concurrent ATC3 medication classes on at least
90 total days during the 12 months before the index date.** The days need not
be consecutive. This counts classes active at the same time, rather than all
distinct drugs or fills observed during the year.

For index date `I`:

```text
window_start   = I minus 12 calendar months
window_end     = I minus 1 day
exposure_start = fill_date
exposure_end   = fill_date + days_supply - 1
```

All boundaries are inclusive. Exposure is clipped to the window, excluding
index day. Calendar-month subtraction uses the last valid day when needed:
February 29, 2024 has a window starting February 28, 2023.

`DAYS_SUPPLY` estimates how long a dispensing will last. A 30-day fill on
June 20 covers June 20 through July 19. If the window starts July 1, it
contributes 19 days. Earlier fills are included when their supply overlaps the
window, with no additional historical fill-date cutoff or cap on positive days
supply.

Multiple fills in the same ATC3 class count once per day. Overlapping and
adjacent intervals are merged without stockpiling refill days. Antiretroviral
therapy is included. Unmapped drugs contribute no class exposure and are counted
separately in the results. Dispensing indicates supply, not confirmed ingestion.

## Inputs

Supply a Redshift cohort with one row per analysis unit:

| Column | Meaning |
|---|---|
| `index_id` | Unique, nonmissing analysis-unit identifier |
| `patient_id` | Komodo longitudinal patient identifier |
| `index_date` | Index date, preferably a Redshift `DATE`. |

Use `patient_id` as the index key when there is one index per patient. For an
episode-based analysis, use the `episode_id` generated during cohort
construction. Multiple index dates per patient require distinct keys. Set
`index_id_column` to that existing column name; the scripts read it as
`index_id` internally and do not generate it.

The default source is `komodo_202610.pharmacy_events`. Required fields are
`patient_id`, `pharmacy_event_id`, `fill_date`, `ndc11`, `days_supply`,
`transaction_result`, `transaction_status`, and `transaction_source_type`.
Retained fills have `transaction_result = 'PAID'`, terminal status `FINAL` or
`STANDALONE`, an 11-digit character NDC11, and positive days supply. Leading
zeros are preserved. Identical source records are collapsed by patient/event;
one fill can contribute to several index windows. Cohort eligibility and
pharmacy observability must be established upstream. A zero result means no
qualifying exposure was observed.

## Files and processing flow

| File | Purpose |
|---|---|
| `1_configure_polypharmacy.R` | Settings and shared functions; loaded automatically by each R stage. Performs no database work when sourced. |
| `2_prepare_polypharmacy_inputs.R` | Saves index windows and cleaned fills; exports unique NDC11 codes. |
| `2.5_map_ndc11_to_atc.ps1` | Runs mapping for the selected version. |
| `n2c_mapper.py` | n2c-based NDC-to-RxCUI-to-ATC4 mapper with caching and provenance. |
| `3_build_polypharmacy_exposures.R` | Stages the crosswalk, derives ATC3, clips exposure, and merges same-class intervals. |
| `4_calculate_and_validate_polypharmacy.R` | Calculates concurrent classes, qualifying days, and final results; exports aggregate summaries. |
| `LICENSE-n2c.txt` | Original n2c MIT license. |
| `0.01_test_polypharmacy.py` | Synthetic developer tests, separate from the analysis run order. |

```text
eligible cohort and index dates
  -> lookback windows and overlapping paid fills
  -> unique NDC11 codes and a saved NDC11-to-ATC4 crosswalk
  -> merged ATC3 exposure intervals within each window
  -> concurrent class counts and qualifying days
  -> one polypharmacy result per input unit
```

ATC3 is the first four characters of a valid five-character ATC4 code, for
example `A10BA -> A10B`. The default `atcprod_only` policy uses product-specific
ATCPROD relations, which account for strength, dose form, and combinations.
Products without an ATCPROD mapping remain unmapped; there is no
ingredient-based ATC fallback. Returned ATCPROD mappings are retained rather
than expanding combination products into separate ingredient classes.
Interval change events count days without creating a full patient-day-class table.

## Configure and run

Run from the study root; here, open `Komodo_generalizability/Frailty_Komoto.Rproj`.
To share, copy this entire folder and adjust `bundle` to its new location.

In **R**, replace the cohort table and select a mapping-version label. Use the
same label in the mapping command; the label below is an example.

```r
bundle <- "Code/shareable_clinical_metrics/polypharmacy"

options(komodo.shareable_polypharmacy.config = list(
  study_root = getwd(),
  komodo_schema = "komodo_202610",
  cohort_table = "YOUR_ELIGIBLE_INDEX_COHORT",
  # cohort_schema = "work_your_username",  # defaults to the personal write schema
  index_id_column = "index_id",
  patient_id_column = "patient_id",
  index_date_column = "index_date",
  table_prefix = "share_poly12m_",
  mapping_version = "baseline_atcprod_20261109",
  relation_policy = "atcprod_only",
  replace_existing = FALSE
))

source(file.path(bundle, "2_prepare_polypharmacy_inputs.R"))
```

Credentials come from `keyring` entries `db_username` and `db_password`; the
write schema defaults to `work_<db_username>`. Set `jdbc_jar_folder` if
`DATABASECONNECTOR_JAR_FOLDER` is not configured. The source-schema fallback
respects `options(komodo.schema = ...)` and `KOMODO_SCHEMA`; an explicit bundle
`komodo_schema` option takes precedence.

Next, in **PowerShell**, from the same study root:

```powershell
powershell.exe -NoProfile -ExecutionPolicy Bypass `
  -File '.\Code\shareable_clinical_metrics\polypharmacy\2.5_map_ndc11_to_atc.ps1' `
  -StudyRoot (Get-Location).Path `
  -MappingVersion 'baseline_atcprod_20261109' `
  -RelationPolicy 'atcprod_only'
```

The execution-policy setting applies only to this PowerShell process.

Alternatively, run the Python mapper directly, including on macOS/Linux:

```text
python Code/shareable_clinical_metrics/polypharmacy/n2c_mapper.py --input Outputs/4_shareable_polypharmacy/unique_ndc11.txt --output-dir Outputs/4_shareable_polypharmacy/mapping/baseline_atcprod_20261109 --mapping-version baseline_atcprod_20261109 --relation-policy atcprod_only
```

Then return to the **same configured R session**:

```r
source(file.path(bundle, "3_build_polypharmacy_exposures.R"))
source(file.path(bundle, "4_calculate_and_validate_polypharmacy.R"))
```

Interrupted mapping can resume with the same command and cache, including an
interruption while publishing completed files. Recovery verifies the input,
mapping version, policy, and crosswalk checksum against prepared metadata.
Completed mappings are reused for the same input, policy, and version.
Choose a new version when refreshing terminology, changing policy, or changing
the exported NDC input. Older combined ATC/ATCPROD crosswalks cannot be reused
as ATCPROD-only mappings.

For an intentional database rebuild, set `replace_existing = TRUE`. Cohort or
lookback changes require stage 2, mapping if the NDC input changes, then stages
3 and 4. A mapping refresh requires mapping, then stages 3 and 4. No report
rendering is required.

Stage 2 assigns an extraction ID and saves the actual lookback setting.
Stage 3 assigns an exposure-build ID. Rebuilds mark downstream results stale
before replacing tables; failed builds remain incomplete. Only results with
`metrics_status = 'READY'` in `share_poly12m_pipeline_state` are current.
Later stages reject stale builds or changed extraction settings. Existing
tables created by the earlier bundle require one rerun of stages 2 through 4
with the updated scripts; mapping is required for the new ATCPROD policy.

## Mapping time and reproducibility

**NDC-to-ATC mappings can change as RxNorm and ATC resources are updated, so
mapping the same NDC codes at different times may produce different crosswalks.**
Apply one saved crosswalk to all index dates in an analysis. Historical fill
dates do not automatically select historical classifications, and some
historical NDCs may remain unmapped.

`manifest.json` records the relationship-source policy, UTC run timestamps, RxNorm/API and ATC/ATCPROD versions
at the start and end, the n2c revision, code/input/output checksums, and the
earliest cached lookup. Cache entries are separated by terminology
version. If versions change during a run, the manifest records both and the
tool prints a message. Preserve the crosswalk and manifest together; a mapping
label or run date alone does not establish a terminology snapshot.

## Database and local outputs

With the default prefix, persistent tables are:

| Table | Grain and purpose |
|---|---|
| `share_poly12m_index_windows` | One input analysis unit and its lookback dates. |
| `share_poly12m_fills` | One analysis unit and retained pharmacy event. |
| `share_poly12m_crosswalk` | One NDC11/ATC4 mapping, or an explicit unmapped NDC11. |
| `share_poly12m_intervals` | One merged ATC3 exposure interval per analysis unit. |
| `share_poly12m_metrics` | One final result per input analysis unit. |
| `share_poly12m_pipeline_state` | One workflow state: extraction settings and input/exposure/result readiness. |

Use a distinct table prefix for another study or concurrent analysis.

Final results include identifiers and dates; window length; qualifying and
any-exposure days; polypharmacy status; concurrent/distinct ATC3 counts;
mapped/unmapped fill counts; extraction/exposure IDs; and definition/mapping settings and versions.
The reported lookback comes from the saved index windows.
`days_5plus_atc3` follows `minimum_classes` if that threshold is changed.
No-fill units remain in the results with zero observed exposure.

Local files are under `Outputs/4_shareable_polypharmacy/`: the NDC11 list,
extraction/mapping summaries, prevalence summary, and qualifying-days distribution.
Fill summaries count index-linked fills. Mapping files are in
`mapping/<mapping_version>/`: `ndc11_atc4.csv`, `manifest.json`, and
`rxnav_cache.sqlite`. Replaced summaries and NDC lists move to
`Outputs/archive/shareable_polypharmacy/<timestamp>/`.
`3_mapping_by_fill_year.csv` reports mapping coverage by original fill year,
counting each patient/event/fill once across index windows.

## References

- [n2c, Fabrício Kury](https://github.com/fabkury/n2c), upstream revision
  `f7d99340c20c16a54a0aef2564c79385351180e7`.
- [NLM RxNorm identifier lookup](https://lhncbc.nlm.nih.gov/RxNav/APIs/api-RxNorm.findRxcuiById.html).
- [NLM RxClass drug-class lookup](https://lhncbc.nlm.nih.gov/RxNav/APIs/api-RxClass.getClassByRxNormDrugId.html).
- [NLM ATCPROD product classification](https://lhncbc.nlm.nih.gov/RxNav/applications/RxClass-ATC-product-classes-ATCPROD.html).
- [NLM RxNorm version endpoint](https://lhncbc.nlm.nih.gov/RxNav/APIs/api-RxNorm.getRxNormVersion.html).
- [NLM RxClass source-version endpoint](https://lhncbc.nlm.nih.gov/RxNav/APIs/api-RxClass.getRelaSourceVersion.html).
- [RxNav terms of service](https://lhncbc.nlm.nih.gov/RxNav/TermsofService.html).
