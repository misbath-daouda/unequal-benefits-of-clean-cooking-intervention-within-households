# ==============================================================================
# 02_exposure_models.R
# Effect of the LPG intervention on 72-h personal CO exposure, by gender.
# Reproduces Table 2 of the manuscript (baseline-adjusted ANCOVA and endline
# mixed models, each with and without covariate adjustment), the treatment x
# gender interaction reported in Supplementary Table 5, and the absolute (ppm)
# reductions quoted in the text. The within-household paired contrasts and the
# two-way fixed-effects specification, which are not reported in the paper, are
# not included in this code release.
# Run from this folder (after editing config.R):  Rscript 02_exposure_models.R
# Inputs : <DATA_DIR>/analysis_files/enrol_exposure_pick_up_data_balance.csv
#          <DATA_DIR>/targeting_final_sample.csv  (wealth index)
# Outputs: <OUTPUT_DIR>/paper/table2_exposure_effects.csv       formatted Table 2
#          <OUTPUT_DIR>/paper/table2_exposure_effects_full.csv  estimates, CIs and the interaction (Supplementary Table 5)
#          <OUTPUT_DIR>/paper/exposure_absolute_reductions.csv
#          <OUTPUT_DIR>/paper/exposure_models_summary.txt
# ==============================================================================
source("config.R")   # DATA_DIR, OUTPUT_DIR, data_path(), out_path() — edit config.R before running
suppressPackageStartupMessages({ library(tidyverse); library(lme4) })
dir.create(out_path("paper"), showWarnings = FALSE, recursive = TRUE)
sink_file <- out_path("paper/exposure_models_summary.txt")
sink(sink_file, split = TRUE)
cat("02_exposure_models.R  —  run", format(Sys.time()), "\n\n")

# ── 1. Data ───────────────────────────────────────────────────────────────────
wealth_lkp <- read.csv(data_path("targeting_final_sample.csv")) |>
  select(hh_id_ref = hhid, ses, wealth) |> distinct(hh_id_ref, .keep_all = TRUE)

exp_dat <- read.csv(data_path("analysis_files/enrol_exposure_pick_up_data_balance.csv")) |>
  select(-starts_with("Unnamed")) |>
  left_join(wealth_lkp, by = "hh_id_ref")

data <- exp_dat |>
  filter(!is.na(mean_session_co_72)) |>
  mutate(log_co = log(mean_session_co_72),
         study_arm = relevel(factor(study_arm), ref = "Control"),
         group     = relevel(factor(group), ref = "F"),
         wealth    = factor(wealth),
         hh_arrangement    = factor(hh_arrangement),
         primary_cookstove = factor(primary_cookstove))

cat("Valid CO sessions:", nrow(data), " participants:", n_distinct(data$study_id),
    " households:", n_distinct(data$hh_id), "\n")
print(with(data, table(study_arm, group, study_period)))

endline_dat <- data |>
  filter(!is.na(wealth), !is.na(hh_arrangement), !is.na(primary_cookstove)) |>
  filter(study_period == "endline", is.finite(log_co))
baseline_vals <- data |> filter(study_period == "baseline", is.finite(log_co)) |>
  select(study_id, baseline_logCO = log_co)
ancova_dat <- endline_dat |> left_join(baseline_vals, by = "study_id") |>
  filter(is.finite(baseline_logCO))
cat("\nEndline model n =", nrow(endline_dat), "; ANCOVA n =", nrow(ancova_dat), "\n")

# ── 2. Mixed models: four specifications of Table 2 ───────────────────────────
m1 <- lmer(log_co ~ study_arm * group + (1 | hh_id), data = endline_dat, REML = TRUE)
m2 <- lmer(log_co ~ study_arm * group + wealth + hh_arrangement + primary_cookstove + (1 | hh_id), data = endline_dat)
m3 <- lmer(log_co ~ study_arm * group + baseline_logCO + (1 | hh_id), data = ancova_dat)
m4 <- lmer(log_co ~ study_arm * group + baseline_logCO + wealth + hh_arrangement + primary_cookstove + (1 | hh_id), data = ancova_dat)

pct <- function(est, se) c(pct = (exp(est) - 1) * 100,
                           lo  = (exp(est - 1.96 * se) - 1) * 100,
                           hi  = (exp(est + 1.96 * se) - 1) * 100)

extract_all <- function(m, label) {
  ct <- coef(summary(m)); V <- vcov(m)
  a  <- "study_armTreatment"; i <- "study_armTreatment:groupM"
  f  <- pct(ct[a, 1], ct[a, 2])
  mm <- pct(ct[a, 1] + ct[i, 1], sqrt(V[a, a] + V[i, i] + 2 * V[a, i]))
  d  <- pct(ct[i, 1], ct[i, 2])
  z  <- ct[i, 1] / ct[i, 2]
  # likelihood-ratio test for the interaction (ML fits)
  m_ml  <- refitML(m)
  m_ml0 <- update(m_ml, . ~ . - study_arm:group)
  lrt_p <- anova(m_ml0, m_ml)$`Pr(>Chisq)`[2]
  tibble(model = label, n = nobs(m),
         female_pct = f["pct"], female_lo = f["lo"], female_hi = f["hi"],
         male_pct = mm["pct"], male_lo = mm["lo"], male_hi = mm["hi"],
         diff_pct = d["pct"], diff_lo = d["lo"], diff_hi = d["hi"],
         diff_log = ct[i, 1], diff_se = ct[i, 2],
         diff_p_wald = 2 * pnorm(-abs(z)), diff_p_lrt = lrt_p)
}

res <- bind_rows(
  extract_all(m3, "ANCOVA unadjusted (primary)"),
  extract_all(m4, "ANCOVA adjusted (primary)"),
  extract_all(m1, "Endline unadjusted"),
  extract_all(m2, "Endline adjusted")
)
fmt <- function(p, l, h) sprintf("%.1f (%.1f, %.1f)", p, l, h)
table2 <- res |> transmute(
  model, n,
  women = fmt(female_pct, female_lo, female_hi),
  men   = fmt(male_pct, male_lo, male_hi),
  men_minus_women = fmt(diff_pct, diff_lo, diff_hi),
  p_interaction_wald = signif(diff_p_wald, 2), p_interaction_lrt = signif(diff_p_lrt, 2))
cat("\n── Table 2: % change in geometric-mean 72-h CO vs control ──\n")
print(as.data.frame(table2), row.names = FALSE)
write.csv(table2, out_path("paper/table2_exposure_effects.csv"), row.names = FALSE)
write.csv(res,    out_path("paper/table2_exposure_effects_full.csv"), row.names = FALSE)

# ── 3. Absolute (ppm) reductions implied by the primary model ─────────────────
# geometric-mean endline CO in the control arm by gender, and model-implied treated level
gm <- endline_dat |> group_by(group) |> summarise(gm_control = exp(mean(log_co[study_arm == "Control"])),
                                                  median_control = median(mean_session_co_72[study_arm == "Control"]), .groups = "drop")
prim <- res |> filter(model == "ANCOVA adjusted (primary)")
abs_tab <- gm |> mutate(pct = ifelse(group == "F", prim$female_pct, prim$male_pct),
                        abs_reduction_gm = gm_control * (-pct / 100))
cat("\n── Absolute reductions implied by the primary (ANCOVA adjusted) estimates ──\n")
print(as.data.frame(abs_tab), row.names = FALSE)
write.csv(abs_tab, out_path("paper/exposure_absolute_reductions.csv"), row.names = FALSE)

sink()
cat("Done. Summary written to", sink_file, "\n")
