# Unequal benefits of a clean cooking intervention within households — analysis code

Code to reproduce the quantitative results, tables and data figures in:

> *Unequal benefits and costs of LPG adoption within households: a household-randomized trial with concurrent
> personal CO monitoring of men and women in peri-urban Techiman, Ghana.* 

The study is a two-arm, household-randomized LPG trial (159 charcoal-using households, August–December 2022)
with 72-hour personal carbon monoxide (CO) measurements on one woman and one man per household at baseline and
endline, stove-use monitors on every cooking stove in a subset of households, a baseline questionnaire of
primary cooks and eight gender-stratified focus groups.

## What is in this repository

| File | Purpose |
|---|---|
| `config.R` | **The only file to edit.** Two placeholders: `DATA_DIR` (where the data files live) and `OUTPUT_DIR` (where results are written). |
| `00_run_all.R` | Runs the seven analysis scripts in order (about one minute on a laptop). |
| `01_sums_events.R` | Detects cooking events on every stove temperature logger with the FireFinder algorithm (`sumsarizer`). |
| `02_exposure_models.R` | Effect of the intervention on 72-h personal CO by gender: baseline-adjusted and endline mixed models, with and without covariate adjustment (Table 2); treatment × gender interaction (Supplementary Table 5); absolute reductions quoted in the text. |
| `03_descriptive_tables.R` | Table 1 and the descriptive supplementary tables (participants included/excluded, valid sessions, CO by arm/gender/period, monitoring start day, self-reported stove use, full baseline tables). |
| `04_sums_analysis.R` | Fig. 3: share of cooking events by fuel and the timing of residual charcoal use (90th percentile of event start time; household bootstrap and permutation tests); cooking events per household-week with Poisson rate ratios (Supplementary Table 8). |
| `05_survey_likert.R` | Primary cooks' Likert responses at enrolment (Supplementary Fig. 3, Supplementary Table 9). |
| `06_figure2_exposure.R` | Fig. 2 (exposure distributions, change scores, forest plot of effects). |
| `07_figure1b_calendar.R` | Fig. 1b (cumulative enrolment and monitoring over calendar time) and the sample sizes shown in the Fig. 1a schematic. |
| `session_info.txt` | R and package versions used for the published results. |

Fig. 1a (study-design schematic) and Fig. 4 (synthesis diagram) are drawings, not analysis outputs, and are
not produced by this code. No data are included (see *Data*).

## Requirements

R (version in `session_info.txt`) with `tidyverse`, `lubridate`, `lme4`, `fixest`, `arsenal`, `patchwork`,
`scales` and `sumsarizer` (`remotes::install_github("geocene/sumsarizer")`).

## How to run

1. Open `config.R` and replace the two placeholders:

   ```r
   DATA_DIR   <- "PATH/TO/DATA"     # folder holding the files listed under "Data"
   OUTPUT_DIR <- "PATH/TO/OUTPUT"   # any folder; created if missing
   ```

2. From this folder, run everything:

   ```bash
   Rscript 00_run_all.R
   ```

   or run the scripts one at a time in numerical order (each one sources `config.R`). `01_sums_events.R` must
   run before `04_sums_analysis.R`, `02_exposure_models.R` before `06_figure2_exposure.R`, and `04` before
   `07`, which read the pipeline's own outputs.

## Data

The scripts expect the following files under `DATA_DIR`. Each script header lists which of them it reads.

| Path under `DATA_DIR` | Content |
|---|---|
| `analysis_files/enrol_exposure_pick_up_data_balance.csv` | One row per participant and monitoring session: 72-h mean CO, validity flags, session dates, study arm, gender, household identifiers and baseline covariates. |
| `analysis_files/enrol_data.csv` | Baseline household questionnaire (one row per household, answered by the primary cook at enrolment), including the Likert items. |
| `targeting_final_sample.csv` | Sampling frame (household id, socio-economic status, wealth index). |
| `sample/first_tranche_exp_sample.csv`, `sample/second_tranche_exp_sample.csv`, `sample/third_tranche_exp_sample.csv` | Enrolment tranches with each household's closest LPG depot. |
| `complete_data/sums_data_csv/converted SUM csv/` | Stove-use monitor exports: one CSV per Lascar EL-USB-TC logger download (Index, Timestamp, Thermocouple °C), file names carrying the household id and stove type. |
| `processed_data/sums/` | Written by `01_sums_events.R` (detected events and a logger inventory) and read by `04_sums_analysis.R`. |


## Outputs

Everything is written under `OUTPUT_DIR/paper/`.

| Manuscript item | Script | Output file(s) |
|---|---|---|
| Table 1 (baseline characteristics by arm) | `03_descriptive_tables.R` | `table1_condensed_counts.csv`, `table1_pvalues.csv` |
| Table 2 (LPG effect on 72-h CO by gender, four specifications) | `02_exposure_models.R` | `table2_exposure_effects.csv`, `exposure_models_summary.txt` |
| Fig. 1b and the sample sizes in Fig. 1a | `07_figure1b_calendar.R` | `fig1b_calendar.png/.pdf`, `fig1_sample_sizes.csv` |
| Fig. 2 | `06_figure2_exposure.R` | `fig2_exposure.png/.pdf` |
| Fig. 3 | `01_sums_events.R` → `04_sums_analysis.R` | `fig3_stove_use.png/.pdf`, `sums_stacking_post_*.csv`, `sums_timing_*.csv` |
| Supplementary Table 1 (included vs excluded participants) | `03_descriptive_tables.R` | `suppl_table1_included_excluded.csv` |
| Supplementary Table 2 (valid CO sessions) | `03_descriptive_tables.R` | `suppl_table2_valid_sessions.csv` |
| Supplementary Table 3 (CO by arm, gender and period) | `03_descriptive_tables.R` | `suppl_table3_co_summary.csv` |
| Supplementary Table 4 (monitoring start day) | `03_descriptive_tables.R` | `suppl_table4_start_day.csv` |
| Supplementary Table 5 (treatment × gender interaction) | `02_exposure_models.R` | `table2_exposure_effects_full.csv` |
| Supplementary Table 6 (self-reported stove use) | `03_descriptive_tables.R` | `suppl_table5_stacking.csv` |
| Supplementary Table 8 (cooking events per household-week) | `04_sums_analysis.R` | `suppl_table_events_per_week_post.csv`, `sums_events_per_week_tests.csv` |
| Supplementary Table 9 and Supplementary Fig. 3 (Likert responses) | `05_survey_likert.R` | `table_likert_*.csv`, `fig4_likert.png/.pdf` |
| Supplementary Tables 12–13 (full baseline tables) | `03_descriptive_tables.R` | `suppl_table7_households_full.csv`, `suppl_table8_participants_full.csv` (file names predate the final table numbering) |
| Absolute CO reductions quoted in the text | `02_exposure_models.R` | `exposure_absolute_reductions.csv` |

Not produced by this code: Table 3 and Supplementary Table 7 (focus-group themes and quotations, coded in
NVivo); Supplementary Table 10 (fuel costs, from the Energy Commission of Ghana); Supplementary Table 11
(ABODE scenarios, run in the ABODE web application with the inputs stated in the Methods); Fig. 1a and Fig. 4.

## Method notes

* Cooking-event detection: FireFinder thresholds of 45 °C (LPG), 55 °C (charcoal coalpot), 38 °C (improved
  coalpot) and 75 °C (three-stone/firewood); minimum event 10 min; minimum gap between events 30 min; loggers
  with 50% or more implausible readings excluded; duplicate exports of the same logger de-duplicated.
* Exposure models: 72-h mean CO is log-transformed; the primary specification is a baseline-adjusted mixed
  model with a household random intercept (ANCOVA); the secondary specification uses all endline sessions
  without the baseline term. Effects are reported as percentage changes in geometric-mean exposure.
* All random procedures (bootstrap, permutation) are seeded, so re-running reproduces the published values.


