# ==============================================================================
# 07_figure1b_calendar.R
# Fig. 1b | Cumulative enrolment and completion of baseline and endline monitoring
# over calendar time (August–December 2022), by arm, and the sample sizes shown
# in the study-design schematic of Fig. 1a: households and participants by arm,
# valid 72-h CO sessions by arm and period, households with post-intervention
# stove-use data, and the interval between baseline and endline monitoring.
# Inputs: <DATA_DIR>/analysis_files/enrol_exposure_pick_up_data_balance.csv
#         <DATA_DIR>/analysis_files/enrol_data.csv
#         <OUTPUT_DIR>/paper/sums_events_per_week_post.csv (from 04_sums_analysis.R)
# Outputs: <OUTPUT_DIR>/paper/fig1b_calendar.png/.pdf, <OUTPUT_DIR>/paper/fig1_sample_sizes.csv
# Run from this folder (after editing config.R): Rscript 07_figure1b_calendar.R
# ==============================================================================
source("config.R")   # DATA_DIR, OUTPUT_DIR, data_path(), out_path() — edit config.R before running
suppressPackageStartupMessages({ library(tidyverse); library(lubridate) })
dir.create(out_path("paper"), showWarnings = FALSE, recursive = TRUE)

exp <- read.csv(data_path("analysis_files/enrol_exposure_pick_up_data_balance.csv")) |> select(-starts_with("Unnamed")) |>
  mutate(start_date = as.Date(start_date))
enrol <- read.csv(data_path("analysis_files/enrol_data.csv")) |> mutate(enrol_date = as.Date(enrol_date))
arm <- exp |> distinct(hh_id_ref, study_arm)
n_hh <- table(arm$study_arm)
n_part <- exp |> distinct(study_id, study_arm) |> count(study_arm)
valid <- exp |> filter(!is.na(mean_session_co_72)) |> count(study_arm, study_period) |>
  pivot_wider(names_from = study_period, values_from = n)
sums <- read.csv(out_path("paper/sums_events_per_week_post.csv")) |> filter(outcome == "all stoves") |> select(study_arm, n = n_households)
gap <- exp |> filter(!is.na(mean_session_co_72)) |> group_by(study_id, study_arm) |>
  summarise(b = min(start_date[study_period == "baseline"]), e = min(start_date[study_period == "endline"]), .groups = "drop") |>
  mutate(wk = as.numeric(e - b) / 7) |> filter(is.finite(wk))
sizes <- bind_rows(
  tibble(quantity = "households enrolled", study_arm = names(n_hh), value = as.numeric(n_hh)),
  n_part |> transmute(quantity = "participants", study_arm, value = n),
  valid |> pivot_longer(-study_arm, names_to = "period", values_to = "value") |>
    transmute(quantity = paste("valid 72-h CO sessions,", period), study_arm, value),
  sums |> transmute(quantity = "households with >= 72 h of post-intervention stove-use logging", study_arm, value = n),
  tibble(quantity = c("weeks from baseline to endline monitoring, median", "weeks from baseline to endline monitoring, IQR lower",
                      "weeks from baseline to endline monitoring, IQR upper"),
         study_arm = "both", value = c(median(gap$wk), quantile(gap$wk, .25), quantile(gap$wk, .75))))
cat("Sample sizes for the study-design schematic (Fig. 1a):\n"); print(as.data.frame(sizes), row.names = FALSE)
write.csv(sizes, out_path("paper/fig1_sample_sizes.csv"), row.names = FALSE)

# ── b. Calendar time ──────────────────────────────────────────────────────────
arm_cols <- c(Control = "#E07B54", Treatment = "#4A90C4")
cum <- function(df, datecol, what) df |> filter(!is.na(.data[[datecol]])) |> count(study_arm, date = .data[[datecol]]) |>
  group_by(study_arm) |> arrange(date) |> mutate(n = cumsum(n), what = what) |> ungroup()
sess <- exp |> filter(!is.na(mean_session_co_72)) |> group_by(hh_id_ref, study_arm, study_period) |>
  summarise(date = min(start_date, na.rm = TRUE), .groups = "drop")
cd <- bind_rows(cum(enrol |> left_join(arm, by = "hh_id_ref"), "enrol_date", "Households enrolled"),
                cum(sess |> filter(study_period == "baseline"), "date", "Baseline monitoring completed (households)"),
                cum(sess |> filter(study_period == "endline"), "date", "Endline monitoring completed (households)")) |>
  mutate(what = factor(what, levels = c("Households enrolled", "Baseline monitoring completed (households)", "Endline monitoring completed (households)")))
pb <- ggplot(cd, aes(date, n, colour = study_arm, linetype = what)) +
  annotate("rect", xmin = as.Date("2022-11-01"), xmax = as.Date("2022-11-30"), ymin = -Inf, ymax = Inf, fill = "grey92") +
  annotate("text", x = as.Date("2022-11-15"), y = 97, label = "Focus groups", size = 2.6, colour = "grey30") +
  geom_step(linewidth = .7) + scale_colour_manual(values = arm_cols, name = NULL) +
  scale_linetype_manual(values = c("solid", "longdash", "dotted"), name = NULL) +
  scale_x_date(limits = as.Date(c("2022-08-01", "2022-12-31")), date_breaks = "1 month", date_labels = "%b %Y") +
  labs(x = NULL, y = "Cumulative number of households") + theme_minimal(base_size = 9) +
  theme(legend.position = "bottom", legend.box = "vertical", legend.key.width = unit(1.2, "cm"), panel.grid.minor = element_blank(),
        legend.margin = margin(0, 0, 0, 0), legend.spacing.y = unit(0, "cm"))
ggsave(out_path("paper/fig1b_calendar.png"), pb, width = 180, height = 95, units = "mm", dpi = 300, bg = "white")
ggsave(out_path("paper/fig1b_calendar.pdf"), pb, width = 180, height = 95, units = "mm")
cat("Fig. 1b written to output/paper/fig1b_calendar.png and fig1_sample_sizes.csv\n")
