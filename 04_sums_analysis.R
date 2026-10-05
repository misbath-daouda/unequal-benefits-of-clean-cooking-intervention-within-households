# ==============================================================================
# 04_sums_analysis.R
# Stove-use monitor analyses reported in the manuscript (post-intervention window):
#   A. Fuel stacking: share of cooking events on LPG, charcoal and wood stoves, by arm (Fig. 3a)
#   B. Timing of residual charcoal use: cumulative distribution of charcoal cooking
#      events over the day and the 90th percentile of event START time by arm,
#      with a household-cluster bootstrap CI and a permutation P for the arm
#      difference (Fig. 3b)
#   C. Cooking events per household-week, all stoves and charcoal stoves, with
#      Poisson rate ratios (Supplementary Table 8)
# Post-intervention window: from 96 h after the end of the baseline CO session to
#   96 h after the start of the endline session. Monitoring time = union of each
#   household's logger coverage within the window (any stove; charcoal stoves),
#   from <DATA_DIR>/processed_data/sums/sums_file_inventory.csv (01_sums_events.R).
# Analyses not reported in the paper (pre/post difference-in-differences, cooking
#   minutes, event durations, window sensitivity) are not included in this code release.
# Run from this folder (after editing config.R): Rscript 04_sums_analysis.R
# Inputs : <DATA_DIR>/processed_data/sums/sums_events_all.csv and sums_file_inventory.csv
#          <DATA_DIR>/analysis_files/enrol_exposure_pick_up_data_balance.csv
# Outputs: <OUTPUT_DIR>/paper/sums_stacking_post_events.csv, sums_stacking_post_households.csv
#          <OUTPUT_DIR>/paper/sums_timing_post_charcoal.csv, sums_timing_tests.csv, sums_timing_ecdf_data.csv
#          <OUTPUT_DIR>/paper/suppl_table_events_per_week_post.csv (Supplementary Table 8),
#            sums_events_per_week_post.csv (descriptives), sums_events_per_week_tests.csv (rate ratios)
#          <OUTPUT_DIR>/paper/fig3_stove_use.png/.pdf (panels: fig3a_stacking.png, fig3b_charcoal_timing.png)
#          <OUTPUT_DIR>/paper/sums_analysis_summary.txt
# ==============================================================================
source("config.R")   # DATA_DIR, OUTPUT_DIR, data_path(), out_path() — edit config.R before running
suppressPackageStartupMessages({ library(tidyverse); library(lubridate); library(fixest); library(patchwork) })
set.seed(20261002)
dir.create(out_path("paper"), showWarnings = FALSE, recursive = TRUE)
sink(out_path("paper/sums_analysis_summary.txt"), split = TRUE)
cat("04_sums_analysis.R — run", format(Sys.time()), "\n\n")

# ── 1. Events and logger inventory ───────────────────────────────────────────
ev0 <- read.csv(data_path("processed_data/sums/sums_events_all.csv"), stringsAsFactors = FALSE)
ev  <- ev0 |> mutate(start_time = ymd_hms(start_time, quiet = TRUE), stop_time = ymd_hms(stop_time, quiet = TRUE))
cat("Raw detected events:", nrow(ev0), "; unparseable times dropped:", sum(is.na(ev$start_time) | is.na(ev$stop_time)),
    "; without household id dropped:", sum(is.na(ev$household_id)), "\n")
ev <- ev |> filter(!is.na(start_time), !is.na(stop_time), !is.na(household_id)) |>
  mutate(key = paste(sum_number, household_id, stove_type, floor_date(start_time, "minute")))
n_before <- nrow(ev); ev <- ev |> distinct(key, .keep_all = TRUE)
cat("Duplicate events across overlapping logger downloads removed:", n_before - nrow(ev), "\n")
ev <- ev |> mutate(fuel = case_when(stove_type == "lpg" ~ "LPG",
                                    stove_type %in% c("coalpot", "improvecoalpot") ~ "Charcoal",
                                    TRUE ~ "Wood"))
inv <- read.csv(data_path("processed_data/sums/sums_file_inventory.csv"), stringsAsFactors = FALSE) |>
  mutate(first_ts = ymd_hms(first_ts, quiet = TRUE), last_ts = ymd_hms(last_ts, quiet = TRUE)) |>
  filter(status == "processed", !is.na(household_id), !is.na(first_ts), !is.na(last_ts)) |>
  mutate(fuel = case_when(stove_type == "lpg" ~ "LPG", stove_type %in% c("coalpot", "improvecoalpot") ~ "Charcoal", TRUE ~ "Wood"))

# ── 2. Study arms and the post-intervention window (from the CO monitoring sessions) ──
exp <- read.csv(data_path("analysis_files/enrol_exposure_pick_up_data_balance.csv")) |>
  select(hh_id, study_arm, study_period, start_time_co, duration_co) |>
  mutate(start = ymd_hms(start_time_co, quiet = TRUE), end = start + dhours(duration_co)) |>
  filter(!is.na(start))
win <- exp |> group_by(hh_id, study_arm) |> summarise(
  base_end  = suppressWarnings(max(end[study_period == "baseline"])),
  end_start = suppressWarnings(min(start[study_period == "endline"])), .groups = "drop") |>
  mutate(across(c(base_end, end_start), ~ if_else(is.finite(.x), .x, as.POSIXct(NA))),
         post_start = base_end + dhours(96), post_end = end_start + dhours(96))
cat("Households with an endline window:", sum(!is.na(win$post_end)), "\n")

# ── 3. Logger coverage per household in the post window (union of logger intervals) ──
union_hours <- function(s, e) {               # s, e POSIXct vectors
  o <- order(s); s <- s[o]; e <- e[o]; tot <- 0; cs <- s[1]; ce <- e[1]
  if (length(s) > 1) for (i in 2:length(s)) { if (s[i] <= ce) ce <- max(ce, e[i]) else { tot <- tot + as.numeric(difftime(ce, cs, units = "hours")); cs <- s[i]; ce <- e[i] } }
  tot + as.numeric(difftime(ce, cs, units = "hours"))
}
coverage <- function(files, w0, w1) {
  f <- files |> mutate(s = pmax(first_ts, w0), e = pmin(last_ts, w1)) |> filter(e > s)
  if (nrow(f) == 0) return(0); union_hours(f$s, f$e)
}
cov <- win |> filter(!is.na(post_end)) |>
  rowwise() |> mutate(
    files = list(inv |> filter(household_id == hh_id)),
    hrs_any      = coverage(files, post_start, post_end),
    hrs_charcoal = coverage(files |> filter(fuel == "Charcoal"), post_start, post_end)) |>
  ungroup() |> select(-files)

# ── 4. Post-intervention events ───────────────────────────────────────────────
post <- ev |> inner_join(win |> select(hh_id, study_arm, post_start, post_end), by = c("household_id" = "hh_id")) |>
  filter(!is.na(post_end), start_time > post_start, start_time <= post_end)
cat("\nPost-intervention events by arm:\n"); print(table(post$study_arm))

# ── A. Stacking: share of post-intervention events by fuel ───────────────────
stack_ev <- post |> count(study_arm, fuel) |> group_by(study_arm) |> mutate(pct = 100 * n / sum(n), n_events = sum(n)) |> ungroup()
stack_hh <- post |> group_by(study_arm, household_id) |> summarise(share_lpg = mean(fuel == "LPG"), share_charcoal = mean(fuel == "Charcoal"), .groups = "drop") |>
  group_by(study_arm) |> summarise(n_households = n(), mean_share_lpg = 100 * mean(share_lpg), mean_share_charcoal = 100 * mean(share_charcoal), .groups = "drop")
cat("\n── A. Post-intervention cooking events by fuel (event-level %) ──\n"); print(as.data.frame(stack_ev))
cat("Household-level mean shares:\n"); print(as.data.frame(stack_hh))
write.csv(stack_ev, out_path("paper/sums_stacking_post_events.csv"), row.names = FALSE)
write.csv(stack_hh, out_path("paper/sums_stacking_post_households.csv"), row.names = FALSE)

# ── B. Timing of charcoal cooking events (post) ──────────────────────────────
tod <- function(t) hour(t) + minute(t) / 60
ch <- post |> filter(fuel == "Charcoal") |> mutate(start_tod = tod(start_time), end_tod = tod(stop_time))
fmt_hm <- function(x) sprintf("%02d:%02d", floor(x), round((x - floor(x)) * 60))
q90 <- function(df) df |> group_by(study_arm) |> summarise(n_events = n(), n_households = n_distinct(household_id),
                                                           q90_start = quantile(start_tod, .9), q90_end = quantile(end_tod, .9),
                                                           median_start = median(start_tod), pct_start_after_17 = 100 * mean(start_tod >= 17),
                                                           pct_start_after_18 = 100 * mean(start_tod >= 18), .groups = "drop")
timing <- q90(ch) |> mutate(q90_start_hm = fmt_hm(q90_start), q90_end_hm = fmt_hm(q90_end), median_start_hm = fmt_hm(median_start))
cat("\n── B. Charcoal cooking events, post-intervention: timing by arm ──\n"); print(as.data.frame(timing))
# household-cluster bootstrap for the arm difference in the 90th percentile of start time
hh_list <- split(ch, ch$household_id); hh_arm <- sapply(hh_list, function(d) d$study_arm[1])
boot_diff <- replicate(1000, {
  s <- unlist(lapply(c("Control", "Treatment"), function(a) sample(names(hh_list)[hh_arm == a], replace = TRUE)))
  d <- bind_rows(hh_list[s]); q <- tapply(d$start_tod, d$study_arm, quantile, probs = .9); q["Treatment"] - q["Control"] })
obs_diff <- with(timing, q90_start[study_arm == "Treatment"] - q90_start[study_arm == "Control"])
# permutation test: shuffle arm labels across households
perm_diff <- replicate(2000, { pa <- setNames(sample(hh_arm), names(hh_arm))
  d <- ch |> mutate(arm_p = pa[household_id]); q <- tapply(d$start_tod, d$arm_p, quantile, probs = .9); q["Treatment"] - q["Control"] })
perm_p <- mean(abs(perm_diff) >= abs(obs_diff))
# household-level share of charcoal events starting at/after 17:00, by arm
sh17 <- ch |> group_by(study_arm, household_id) |> summarise(p17 = mean(start_tod >= 17), .groups = "drop")
w17 <- wilcox.test(p17 ~ study_arm, data = sh17)
timing_tests <- tibble(statistic = c("difference in 90th percentile of start time, treatment - control (h)",
                                     "bootstrap 95% CI lower", "bootstrap 95% CI upper", "permutation P (2000 reps)",
                                     "household mean share of charcoal events starting >= 17:00, control", "same, treatment",
                                     "Wilcoxon P (household shares)"),
                       value = c(obs_diff, quantile(boot_diff, .025), quantile(boot_diff, .975), perm_p,
                                 mean(sh17$p17[sh17$study_arm == "Control"]), mean(sh17$p17[sh17$study_arm == "Treatment"]), w17$p.value))
cat("\nTiming tests:\n"); print(as.data.frame(timing_tests))
write.csv(timing, out_path("paper/sums_timing_post_charcoal.csv"), row.names = FALSE)
write.csv(timing_tests, out_path("paper/sums_timing_tests.csv"), row.names = FALSE)
ecdf_dat <- ch |> group_by(study_arm) |> arrange(start_tod) |> mutate(cum = row_number() / n()) |> ungroup()
write.csv(ecdf_dat |> select(study_arm, household_id, start_tod, end_tod, cum), out_path("paper/sums_timing_ecdf_data.csv"), row.names = FALSE)

# ── C. Cooking events per household-week (households with >= 72 h of logger coverage) ──
MIN_HRS <- 72
counts <- post |> group_by(hh_id = household_id, study_arm) |>
  summarise(n_all = n(), n_charcoal = sum(fuel == "Charcoal"), .groups = "drop")
rates <- cov |> select(hh_id, study_arm, hrs_any, hrs_charcoal) |>
  left_join(counts, by = c("hh_id", "study_arm")) |>
  mutate(across(c(n_all, n_charcoal), ~ replace_na(.x, 0L)),
         wk_any = hrs_any / 168, wk_charcoal = hrs_charcoal / 168,
         rate_all = n_all / wk_any, rate_charcoal = n_charcoal / wk_charcoal,
         treat = as.integer(study_arm == "Treatment"))
po <- rates |> filter(!is.na(hrs_any), hrs_any >= MIN_HRS)            # any stove logged >= 72 h
pc <- rates |> filter(!is.na(hrs_charcoal), hrs_charcoal >= MIN_HRS)  # charcoal stoves logged >= 72 h
desc <- po |> group_by(study_arm) |> summarise(
  n_households = n(), median_weeks_monitored = median(wk_any),
  all_median = median(rate_all), all_q1 = quantile(rate_all, .25), all_q3 = quantile(rate_all, .75), all_mean = mean(rate_all), .groups = "drop") |>
  mutate(across(where(is.numeric) & !n_households, ~ round(.x, 2)))
desc_ch <- pc |> group_by(study_arm) |> summarise(
  n_households = n(), charcoal_median = median(rate_charcoal), charcoal_q1 = quantile(rate_charcoal, .25),
  charcoal_q3 = quantile(rate_charcoal, .75), charcoal_mean = mean(rate_charcoal), .groups = "drop") |>
  mutate(across(where(is.numeric) & !n_households, ~ round(.x, 2)))
cat("\n── C. Cooking events per household-week after the intervention (households with >=", MIN_HRS, "h of logger coverage) ──\n")
print(as.data.frame(desc)); cat("Charcoal events per week (households with charcoal loggers):\n"); print(as.data.frame(desc_ch))
# Poisson rate ratios, intervention vs control, with log monitoring time as offset and heteroskedasticity-robust SEs
rr <- function(m, nm) { ct <- coeftable(m); e <- ct[nm, 1]; s <- ct[nm, 2]
  c(RR = exp(e), lo = exp(e - 1.96 * s), hi = exp(e + 1.96 * s), p = ct[nm, 4]) }
m_post    <- fepois(n_all ~ treat, offset = ~ log(wk_any), data = po, vcov = "hetero")
m_ch_post <- fepois(n_charcoal ~ treat, offset = ~ log(wk_charcoal), data = pc, vcov = "hetero")
tests <- bind_rows(
  as_tibble_row(rr(m_post, "treat"))    |> mutate(outcome = "all stoves", n_households = nrow(po), .before = 1),
  as_tibble_row(rr(m_ch_post, "treat")) |> mutate(outcome = "charcoal",   n_households = nrow(pc), .before = 1))
cat("\nRate ratios (intervention vs control):\n"); print(as.data.frame(tests |> mutate(across(c(RR, lo, hi), ~ round(.x, 2)), p = signif(p, 2))))
post_tab <- desc |> transmute(arm = study_arm, households = n_households,
                              events_per_week = sprintf("%.1f (%.1f–%.1f)", all_median, all_q1, all_q3)) |>
  left_join(desc_ch |> transmute(arm = study_arm, charcoal_households = n_households,
                                 charcoal_events_per_week = sprintf("%.1f (%.1f–%.1f)", charcoal_median, charcoal_q1, charcoal_q3)), by = "arm")
write.csv(post_tab, out_path("paper/suppl_table_events_per_week_post.csv"), row.names = FALSE)
write.csv(bind_rows(desc |> mutate(outcome = "all stoves"), desc_ch |> mutate(outcome = "charcoal")),
          out_path("paper/sums_events_per_week_post.csv"), row.names = FALSE)
write.csv(tests, out_path("paper/sums_events_per_week_tests.csv"), row.names = FALSE)

# ── Fig. 3: a (fuel stacking, 100% horizontal bars) above b (timing of charcoal events); 180 x 160 mm ──
arm_cols <- c(Control = "#E07B54", Treatment = "#4A90C4")
pA <- stack_ev |> mutate(fuel = factor(fuel, levels = c("Charcoal", "Wood", "LPG"))) |>
  left_join(stack_hh |> select(study_arm, n_households), by = "study_arm") |>
  mutate(arm_lab = sprintf("%s (n = %d)", ifelse(study_arm == "Treatment", "Intervention", study_arm), n_households)) |>
  ggplot(aes(pct, arm_lab, fill = fuel)) + geom_col(width = 0.62) +
  geom_text(aes(label = ifelse(pct >= 5, sprintf("%.0f%%", pct), "")), position = position_stack(vjust = .5), colour = "white", size = 3.8) +
  scale_fill_manual(values = c(Charcoal = "#8B6B2E", Wood = "#C9A96E", LPG = "#4A90B8"), name = NULL, guide = guide_legend(reverse = TRUE)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.01))) + scale_y_discrete(limits = rev) +
  labs(x = "Share of cooking events (%)", y = NULL) + theme_minimal(base_size = 11) +
  theme(legend.position = "top", legend.justification = "left", panel.grid.major.y = element_blank(), panel.grid.minor = element_blank())
ggsave(out_path("paper/fig3a_stacking.png"), pA, width = 7, height = 2.6, dpi = 300, bg = "white")
lab_dat <- timing |> mutate(y = ifelse(study_arm == "Control", .2, .12))
pB <- ggplot(ecdf_dat, aes(start_tod, cum, colour = study_arm)) + geom_step(linewidth = 1) +
  geom_hline(yintercept = .9, linetype = "dotted", colour = "grey40") +
  geom_vline(data = timing, aes(xintercept = q90_start, colour = study_arm), linetype = "dashed", show.legend = FALSE) +
  geom_label(data = lab_dat, aes(x = q90_start, y = y, label = q90_start_hm, colour = study_arm), size = 3.5, show.legend = FALSE) +
  scale_colour_manual(values = arm_cols, name = NULL,
                      labels = with(timing, sprintf("%s: %d households, %d events", ifelse(study_arm == "Treatment", "Intervention", study_arm), n_households, n_events))) +
  scale_x_continuous(breaks = seq(0, 24, 3), labels = sprintf("%02d:00", seq(0, 24, 3)), limits = c(0, 24)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Hour of day (event start)", y = "Cumulative share of charcoal cooking events") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", legend.direction = "horizontal", panel.grid.minor = element_blank())
ggsave(out_path("paper/fig3b_charcoal_timing.png"), pB, width = 7, height = 4.6, dpi = 300, bg = "white")
fig3 <- (pA / pB) + plot_layout(heights = c(1, 2.6)) + plot_annotation(tag_levels = "a") & theme(plot.tag = element_text(face = "bold"))
ggsave(out_path("paper/fig3_stove_use.png"), fig3, width = 180, height = 160, units = "mm", dpi = 300, bg = "white")
ggsave(out_path("paper/fig3_stove_use.pdf"), fig3, width = 180, height = 160, units = "mm")
sink()
cat("Done. Figures and tables in output/paper/\n")
