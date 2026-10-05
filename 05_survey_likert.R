# ==============================================================================
# 05_survey_likert.R
# Primary cooks' agreement with statements on fuel decision making and the
# value of LPG, asked once in the baseline household questionnaire at ENROLMENT
# (August to October 2022, before randomization). Timing verified 4 Oct 2026 against
# the raw SurveyCTO exports: the form date equals the enrolment date for 147/149
# matched households (two are 7 days earlier), a median 7 days before baseline CO
# monitoring and 61 days before endline; no form is dated on/after the endline
# session and the items are absent from the endline forms. Reproduces the numbers
# quoted in the manuscript and draws Supplementary Fig. 3 (file fig4_likert.png)
# with the Likert levels in their correct order.
#
# Run from this folder (after editing config.R): Rscript 05_survey_likert.R
# Inputs : <DATA_DIR>/analysis_files/enrol_data.csv (baseline form, 1 row per household)
#          <DATA_DIR>/analysis_files/enrol_exposure_pick_up_data_balance.csv (study arm)
# Outputs: <OUTPUT_DIR>/paper/table_likert_by_gender.csv, _by_gender_arm.csv
#          <OUTPUT_DIR>/paper/fig4_likert.png / .pdf
# ==============================================================================
source("config.R")   # DATA_DIR, OUTPUT_DIR, data_path(), out_path() — edit config.R before running
suppressPackageStartupMessages({ library(tidyverse) })
dir.create(out_path("paper"), showWarnings = FALSE, recursive = TRUE)

enrol <- read.csv(data_path("analysis_files/enrol_data.csv")) |> select(-any_of("X"))
arm   <- read.csv(data_path("analysis_files/enrol_exposure_pick_up_data_balance.csv")) |>
  distinct(hh_id_ref, study_arm)
d <- enrol |> left_join(arm, by = "hh_id_ref")
stopifnot(all(d$resp_primary_cook == 1), nrow(d) == 159)
cat("Primary cooks:", nrow(d), " women:", sum(d$resp_gen == "F"), " men:", sum(d$resp_gen == "M"), "\n")
cat("Enrolment dates of these households:", min(d$enrol_date), "to", max(d$enrol_date), "(items answered at enrolment, before randomization)\n")

levels5 <- c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree")
items <- c(
  hh_intr_fuel     = "I can independently decide which type of fuel this household uses for cooking",
  hh_intr_benefits = "In my opinion, the benefits of cooking with LPG outweigh its cost",
  hh_intr_decision = "Women and men have the same right to make decisions about household purchases"
)
# hh_intr_impact, hh_intr_permission and hh_intr_opinion are additional items not shown.

long <- d |>
  select(hh_id_ref, study_arm, gender = resp_gen, all_of(names(items))) |>
  pivot_longer(all_of(names(items)), names_to = "item", values_to = "response") |>
  filter(!is.na(response), response != "") |>
  mutate(response = factor(response, levels = levels5),
         statement = factor(items[item], levels = items),
         gender = factor(gender, levels = c("F", "M"), labels = c("Women", "Men")))
stopifnot(!any(is.na(long$response)))

tab_gender <- long |> count(statement, gender, response, .drop = FALSE) |>
  group_by(statement, gender) |> mutate(n_total = sum(n), pct = 100 * n / n_total) |> ungroup()
tab_arm <- long |> count(statement, gender, study_arm, response, .drop = FALSE) |>
  group_by(statement, gender, study_arm) |> mutate(n_total = sum(n), pct = 100 * n / n_total) |> ungroup()
write.csv(tab_gender, out_path("paper/table_likert_by_gender.csv"), row.names = FALSE)
write.csv(tab_arm, out_path("paper/table_likert_by_gender_arm.csv"), row.names = FALSE)

cat("\n── Agreement by gender (row %) ──\n")
print(tab_gender |> select(statement, gender, response, n, pct) |>
        mutate(pct = round(pct, 1)) |>
        pivot_wider(names_from = response, values_from = c(n, pct)) |> as.data.frame())
summ <- tab_gender |> group_by(statement, gender, n_total) |>
  summarise(agree_or_strongly = sum(pct[response %in% c("Agree", "Strongly agree")]),
            strongly_agree = sum(pct[response == "Strongly agree"]),
            disagree_or_strongly = sum(pct[response %in% c("Disagree", "Strongly disagree")]),
            strongly_disagree = sum(pct[response == "Strongly disagree"]), .groups = "drop") |>
  mutate(across(where(is.numeric) & !n_total, ~ round(.x, 1)))
cat("\n── Summary shares ──\n"); print(as.data.frame(summ))
write.csv(summ, out_path("paper/table_likert_summary.csv"), row.names = FALSE)

# by arm among women (enrolment responses, before randomization: a balance check, not a treatment contrast)
cat("\n── Women, by study arm: agree or strongly agree (%) ──\n")
print(tab_arm |> filter(gender == "Women") |> group_by(statement, study_arm, n_total) |>
        summarise(agree_or_strongly = round(sum(pct[response %in% c("Agree", "Strongly agree")]), 1),
                  strongly_disagree = round(sum(pct[response == "Strongly disagree"]), 1), .groups = "drop") |>
        as.data.frame())

# ── Supplementary Fig. 3: diverging stacked bars, Likert levels in scale order ──
pal <- c("Strongly disagree" = "#b2182b", "Disagree" = "#ef8a62", "Neither agree nor disagree" = "#d9d9d9",
         "Agree" = "#67a9cf", "Strongly agree" = "#2166ac")
plot_dat <- tab_gender |>
  mutate(gender_lab = paste0(gender, " (n = ", n_total, ")"),
         statement_wrap = str_wrap(as.character(statement), 48))
p <- ggplot(plot_dat, aes(x = pct, y = gender_lab, fill = response)) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(pct >= 6, sprintf("%.0f%%", pct), "")),
            position = position_stack(vjust = 0.5), size = 3, colour = "white") +
  facet_wrap(~ statement_wrap, ncol = 1) +
  scale_fill_manual(values = pal, breaks = levels5, name = NULL) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.01))) +
  labs(x = "Share of primary cooks", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0))
ggsave(out_path("paper/fig4_likert.png"), p, width = 7.2, height = 6, dpi = 300, bg = "white")
ggsave(out_path("paper/fig4_likert.pdf"), p, width = 7.2, height = 6)
cat("\nSupplementary Fig. 3 written to", out_path("paper/fig4_likert.png"), "\n")
