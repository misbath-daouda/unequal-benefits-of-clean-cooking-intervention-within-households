# ==============================================================================
# 06_figure2_exposure.R
# Fig. 2 of the manuscript: personal CO exposure by gender, arm and period.
#   a. distribution of log 72-h CO by gender at baseline and endline
#   b. change in log 72-h CO (endline - baseline) by gender and arm
#   c. treatment effects (% change, 95% CI) by gender across the four specifications
# Inputs: <DATA_DIR>/analysis_files/enrol_exposure_pick_up_data_balance.csv
#         <OUTPUT_DIR>/paper/table2_exposure_effects_full.csv (from 02_exposure_models.R)
# Run from this folder (after editing config.R): Rscript 06_figure2_exposure.R
# ==============================================================================
source("config.R")   # DATA_DIR, OUTPUT_DIR, data_path(), out_path() — edit config.R before running
suppressPackageStartupMessages({ library(tidyverse); library(patchwork) })
dir.create(out_path("paper"), showWarnings = FALSE, recursive = TRUE)
data <- read.csv(data_path("analysis_files/enrol_exposure_pick_up_data_balance.csv")) |> select(-starts_with("Unnamed")) |>
  filter(!is.na(mean_session_co_72)) |> mutate(log_co = log(mean_session_co_72)) |> filter(is.finite(log_co))
arm_cols <- c(Control = "#E07B54", Treatment = "#4A90C4"); gender_cols <- c(Women = "#8E5A9E", Men = "#5B8C3A")
dat <- data |> mutate(gender = factor(group, levels = c("F", "M"), labels = c("Women", "Men")),
                      period = factor(study_period, levels = c("baseline", "endline"), labels = c("Baseline", "Endline")),
                      study_arm = factor(study_arm, levels = c("Control", "Treatment")))
base_theme <- theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank(), legend.position = "bottom",
                                                     strip.text = element_text(face = "bold"))
# a
n_a <- dat |> count(period)
pa <- ggplot(dat, aes(log_co, fill = gender, colour = gender)) + geom_density(alpha = .25, linewidth = .7) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") + facet_wrap(~ period) +
  scale_fill_manual(values = gender_cols, name = NULL) + scale_colour_manual(values = gender_cols, name = NULL) +
  labs(x = "log(72-h mean CO, ppm)", y = "Density") + base_theme +
  theme(legend.position = "inside", legend.position.inside = c(0.22, 0.86), legend.background = element_rect(fill = "white", colour = NA))
# b
wide <- dat |> select(study_id, hh_id, study_arm, gender, period, log_co) |> distinct() |>
  pivot_wider(names_from = period, values_from = log_co) |> filter(is.finite(Baseline), is.finite(Endline)) |>
  mutate(change = Endline - Baseline)
pb <- ggplot(wide, aes(study_arm, change, colour = study_arm)) + geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_boxplot(width = .35, outlier.shape = NA, fill = NA) + geom_jitter(width = .1, alpha = .35, size = .8) +
  facet_wrap(~ gender) + scale_colour_manual(values = arm_cols, guide = "none") +
  labs(x = NULL, y = "Change in log 72-h CO\n(endline − baseline)") + coord_cartesian(ylim = c(-7.5, 7.5)) + base_theme
# c
res <- read.csv(out_path("paper/table2_exposure_effects_full.csv")) |>
  mutate(model = factor(model, levels = rev(c("ANCOVA unadjusted (primary)", "ANCOVA adjusted (primary)", "Endline unadjusted", "Endline adjusted")),
                        labels = rev(c("Baseline-adjusted, unadjusted", "Baseline-adjusted, covariate-adjusted", "Endline-only, unadjusted", "Endline-only, covariate-adjusted"))))
fc <- bind_rows(res |> transmute(model, gender = "Women", pct = female_pct, lo = female_lo, hi = female_hi),
                res |> transmute(model, gender = "Men", pct = male_pct, lo = male_lo, hi = male_hi)) |>
  mutate(gender = factor(gender, levels = c("Women", "Men")))
pc <- ggplot(fc, aes(pct, model, colour = gender)) + geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = .2, position = position_dodge(width = .55)) +
  geom_point(size = 1.8, position = position_dodge(width = .55)) + scale_colour_manual(values = gender_cols, name = NULL) +
  labs(x = "Effect of the LPG intervention on 72-h CO exposure\n(% change vs control, 95% CI)", y = NULL) + base_theme
top <- (pa | pb) + plot_layout(widths = c(1.1, 1.3))
fig2 <- (top / pc) + plot_layout(heights = c(1.6, 1)) + plot_annotation(tag_levels = "a") & theme(plot.tag = element_text(face = "bold"))
ggsave(out_path("paper/fig2_exposure.png"), fig2, width = 7.5, height = 7.2, dpi = 300, bg = "white")
ggsave(out_path("paper/fig2_exposure.pdf"), fig2, width = 7.5, height = 7.2)
cat("Fig. 2 written: n sessions in a =", nrow(dat), "; participants with both periods in b =", nrow(wide), "\n")
