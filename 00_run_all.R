# ==============================================================================
# 00_run_all.R — reproduce every quantitative result, table and data figure of the paper
# Run from this folder after editing config.R:   Rscript 00_run_all.R
# Steps (each script can also be run on its own, in this order):
#   01_sums_events.R        FireFinder cooking-event detection on all stove temperature loggers (about 1 min)
#   02_exposure_models.R    Table 2 (gender-specific LPG effects on 72-h CO), Supplementary Table 5, absolute reductions
#   03_descriptive_tables.R Table 1 and Supplementary Tables 1-4, 6, 12-13
#   04_sums_analysis.R      Fig. 3 (fuel stacking, charcoal timing) and Supplementary Table 8 (events per household-week)
#   05_survey_likert.R      Supplementary Fig. 3 and Supplementary Table 9 (primary cooks' Likert responses at enrolment)
#   06_figure2_exposure.R   Fig. 2 (exposure distributions, change scores, forest plot)
#   07_figure1b_calendar.R  Fig. 1b (calendar-time enrolment) and the sample sizes shown in the Fig. 1a schematic
# Outputs go to <OUTPUT_DIR>/paper/. Fig. 1a and Fig. 4 are schematic diagrams drawn manually and are
# not produced by code; the ABODE scenario of Supplementary Table 11 is run in the ABODE web application.
# ==============================================================================
source("config.R")   # DATA_DIR, OUTPUT_DIR, data_path(), out_path() — edit config.R before running
scripts <- c("01_sums_events.R", "02_exposure_models.R", "03_descriptive_tables.R", "04_sums_analysis.R",
             "05_survey_likert.R", "06_figure2_exposure.R", "07_figure1b_calendar.R")
for (s in scripts) {
  cat("\n==================== ", s, " ====================\n", sep = "")
  t0 <- Sys.time()
  source(s, echo = FALSE, local = new.env())
  cat(sprintf("[%s finished in %.1f min]\n", s, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
cat("\nAll done. See", file.path(OUTPUT_DIR, "paper"), "\n")
