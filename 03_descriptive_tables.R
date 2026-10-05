# ==============================================================================
# 03_descriptive_tables.R
# Reproduces the descriptive tables of the manuscript from the analysis files:
#   Table 1 (condensed household + participant characteristics by arm)
#   Supplementary Table 1 (included vs excluded participants)
#   Supplementary Table 2 (valid CO sessions by arm and period)
#   Supplementary Table 3 (72-h CO by arm, gender and period)
#   Supplementary Table 4 (monitoring start day by arm)
#   Supplementary Table 5 (self-reported stacking, post-intervention)
#   Supplementary Tables 7-8 (full household and participant tables)
# Writes one CSV file per table.
# Run from this folder (after editing config.R):  Rscript 03_descriptive_tables.R
# ==============================================================================
source("config.R")   # DATA_DIR, OUTPUT_DIR, data_path(), out_path() — edit config.R before running
suppressPackageStartupMessages({ library(tidyverse); library(lubridate); library(arsenal) })
dir.create(out_path("paper"), showWarnings = FALSE, recursive = TRUE)
to_df <- function(tab) as.data.frame(summary(tab, text = TRUE, digits = 1))

# ── Data ────────────────────────────────────
enrol_data     <- read.csv(data_path("analysis_files/enrol_data.csv")) |> select(-any_of("X"))
enrol_exp_data <- read.csv(data_path("analysis_files/enrol_exposure_pick_up_data_balance.csv")) |> select(-starts_with("Unnamed"))
sampling_list  <- read.csv(data_path("targeting_final_sample.csv")) |> select(hh_id_ref = hhid, ses, wealth) |> distinct(hh_id_ref, .keep_all = TRUE)
sample_combined <- map_dfr(c(data_path("sample/first_tranche_exp_sample.csv"), data_path("sample/second_tranche_exp_sample.csv"),
                             data_path("sample/third_tranche_exp_sample.csv")), ~ read.csv(.x) |> select(hh_id_ref = hhid, closest_depot)) |>
  distinct(hh_id_ref, .keep_all = TRUE) |> mutate(closest_depot = as.factor(closest_depot))
lookup_joins <- function(df) df |> left_join(sampling_list, by = "hh_id_ref") |> left_join(sample_combined, by = "hh_id_ref")
enrol_data <- lookup_joins(enrol_data); enrol_exp_data <- lookup_joins(enrol_exp_data)

valid_co_lookup <- enrol_exp_data |> group_by(study_id) |>
  summarise(valid_co = factor(any(!is.na(mean_session_co_72)), levels = c(TRUE, FALSE), labels = c("Valid CO", "Missing CO")), .groups = "drop")
data <- enrol_exp_data |> filter(!is.na(mean_session_co_72)) |>
  mutate(start_date = as.Date(start_date), start_day_of_week = wday(start_date, label = TRUE),
         log_co = log(mean_session_co_72))
study_info <- data |> distinct(hh_id_ref, study_arm)
hh <- enrol_data |> filter(hh_id_ref %in% data$hh_id_ref) |> left_join(study_info, by = "hh_id_ref") |>
  mutate(secondary_cookstove = replace_na(secondary_cookstove, "None"), tertiary_cookstove = replace_na(tertiary_cookstove, "None"),
         secondary_cookstove = fct_relevel(factor(secondary_cookstove), "None"),
         tertiary_cookstove = fct_relevel(factor(tertiary_cookstove), "None"),
         hh_arrangement = fct_relevel(factor(hh_arrangement), "Mother and children", "Father and children"),
         relation_to_sec_resp = fct_relevel(relation_to_sec_resp, "Other", after = Inf))
cat("Households:", nrow(hh), " by arm:", paste(names(table(hh$study_arm)), table(hh$study_arm), collapse = ", "), "\n")

ctrl <- tableby.control(numeric.stats = c("median", "q1q3"), cat.stats = "countpct",
                        stats.labels = list(median = "Median", q1q3 = "Q1,Q3"), total = TRUE, test.always = TRUE)
labs <- function(df, l) { for (v in names(l)) attr(df[[v]], "label") <- l[[v]]; df }
hh <- labs(hh, list(hh_mems_total = "Household size", hh_arrangement = "Household composition", wealth = "Wealth index",
                    primary_cookstove = "Primary cookstove", secondary_cookstove = "Secondary cookstove",
                    tertiary_cookstove = "Tertiary cookstove", total_cookstoves = "Total number of cookstoves",
                    relation_to_sec_resp = "Relationship between primary cook and second participant", closest_depot = "Neighborhood"))
tab_hh <- tableby(study_arm ~ hh_mems_total + hh_arrangement + wealth + primary_cookstove + secondary_cookstove +
                    tertiary_cookstove + total_cookstoves + relation_to_sec_resp + closest_depot, data = hh, control = ctrl)
write.csv(to_df(tab_hh), out_path("paper/suppl_table7_households_full.csv"), row.names = FALSE)

# participants (valid CO only; n = 313)
extract_participant <- function(df, prefix, is_primary) {
  df |> filter(substr(study_id, nchar(study_id), nchar(study_id)) == .data[[paste0(prefix, "_gen")]]) |>
    select(study_id, study_arm, starts_with(paste0(prefix, "_"))) |>
    rename_with(~ gsub(paste0("^", prefix, "_"), "", .), starts_with(paste0(prefix, "_"))) |>
    distinct(study_id, .keep_all = TRUE) |> mutate(primary_cook = if_else(is_primary, "Yes", "No"))
}
participant_data <- data |> select(study_id, study_arm, contains("_age"), contains("_gen"), contains("_education"),
                                   contains("_occupation"), contains("religion")) |> distinct(study_id, .keep_all = TRUE)
p_data <- bind_rows(extract_participant(participant_data, "resp", TRUE), extract_participant(participant_data, "resp2", FALSE)) |>
  mutate(occupation = fct_relevel(factor(occupation), "Does not work", "Construction", "Domestic work",
                                  "Farming or agricultural work", "Professional", "Trading", "Transportation", "Other"))
p_data <- labs(p_data, list(age = "Age", gen = "Gender", education = "Education", occupation = "Occupation",
                            religion = "Religion", primary_cook = "Primary cook"))
cat("Participants with valid CO:", nrow(p_data), "\n")
tab_p <- tableby(study_arm ~ age + gen + education + occupation + religion + primary_cook + interaction(primary_cook, gen),
                 data = p_data, control = ctrl)
write.csv(to_df(tab_p), out_path("paper/suppl_table8_participants_full.csv"), row.names = FALSE)

# Supplementary Table 1: included vs excluded
participant_raw <- enrol_exp_data |> select(study_id, study_arm, contains("_age"), contains("_gen"), contains("_education"),
                                            contains("_occupation"), contains("religion")) |> distinct(study_id, .keep_all = TRUE)
attrition <- bind_rows(extract_participant(participant_raw, "resp", TRUE), extract_participant(participant_raw, "resp2", FALSE)) |>
  left_join(valid_co_lookup, by = "study_id") |>
  mutate(occupation = fct_relevel(factor(occupation), "Does not work", "Construction", "Domestic work",
                                  "Farming or agricultural work", "Professional", "Trading", "Transportation", "Other"))
attrition <- labs(attrition, list(age = "Age", gen = "Gender", education = "Education", occupation = "Occupation",
                                  religion = "Religion", primary_cook = "Primary cook", study_arm = "Study arm"))
tab_att <- tableby(valid_co ~ age + gen + education + occupation + religion + primary_cook + study_arm, data = attrition, control = ctrl)
write.csv(to_df(tab_att), out_path("paper/suppl_table1_included_excluded.csv"), row.names = FALSE)
cat("Enrolled participants:", nrow(attrition), " with valid CO:", sum(attrition$valid_co == "Valid CO"), "\n")

# Supplementary Table 2: sessions with valid CO by arm and period
sess <- data |> count(study_arm, study_period) |> group_by(study_arm) |> mutate(pct = round(100 * n / sum(n), 1)) |> ungroup()
chi  <- chisq.test(table(data$study_arm, data$study_period))
sess$chisq_p <- chi$p.value
write.csv(sess, out_path("paper/suppl_table2_valid_sessions.csv"), row.names = FALSE)
cat("Valid sessions:", nrow(data), " chi-squared p =", round(chi$p.value, 3), "\n")

# Supplementary Table 3: CO by arm, gender, period
summ <- function(df) df |> summarise(n = n(), mean = mean(mean_session_co_72), sd = sd(mean_session_co_72),
                                     median = median(mean_session_co_72), iqr = IQR(mean_session_co_72),
                                     log_mean = mean(log_co[is.finite(log_co)]), log_sd = sd(log_co[is.finite(log_co)]),
                                     log_median = median(log_co[is.finite(log_co)]), log_iqr = IQR(log_co[is.finite(log_co)]), .groups = "drop")
s3 <- bind_rows(data |> group_by(period = str_to_title(study_period), study_arm, group) |> summ(),
                data |> group_by(study_arm, group) |> summ() |> mutate(period = "Overall")) |>
  mutate(group = if_else(group == "F", "Female", "Male")) |> relocate(period) |>
  mutate(across(where(is.numeric) & !n, ~ round(.x, 2)))
write.csv(s3, out_path("paper/suppl_table3_co_summary.csv"), row.names = FALSE)
cat("\nSupplementary Table 3 (CO ppm):\n"); print(as.data.frame(s3 |> select(period, study_arm, group, n, mean, sd, median, iqr)))

# Supplementary Table 4: start day of week
data <- data |> mutate(period_day = interaction(study_period, start_day_of_week, sep = ", "))
attr(data$start_day_of_week, "label") <- "Exposure monitoring start day"; attr(data$period_day, "label") <- "Study period * start day"
tab_s4 <- tableby(study_arm ~ start_day_of_week + period_day, data = data, control = tableby.control(cat.stats = "countpct", total = TRUE, test.always = TRUE))
write.csv(to_df(tab_s4), out_path("paper/suppl_table4_start_day.csv"), row.names = FALSE)

# Supplementary Table 5: stacking at endline (one row per household)
endline_stack <- data |> filter(study_period == "endline") |>
  mutate(across(contains("cooking_reg") | contains("cooking_imp") | contains("boiling_reg") | contains("boiling_imp") | contains("any_other"),
                ~ ifelse(. == 0, "No", "Yes"))) |>
  mutate(across(contains("cooking_reg") | contains("cooking_imp"), ~ ifelse(any_other_cooking_stove == "No", NA, .))) |>
  mutate(across(contains("boiling_reg") | contains("boiling_imp"), ~ ifelse(any_other_boiling_stove == "No", NA, .))) |>
  select(hh_id, study_arm, primary_cooking_stove, primary_boiling_stove, contains("cooking_reg") | contains("cooking_imp") |
           contains("boiling_reg") | contains("boiling_imp") | contains("any_other")) |>
  distinct(hh_id, .keep_all = TRUE)
tab_s5 <- tableby(study_arm ~ primary_cooking_stove + any_other_cooking_stove + cooking_reg_charc + cooking_imp_charc + cooking_reg_wood +
                    cooking_imp_wood + primary_boiling_stove + any_other_boiling_stove + boiling_reg_charc + boiling_imp_charc +
                    boiling_reg_wood + boiling_imp_wood, data = endline_stack,
                  control = tableby.control(cat.stats = "countpct", total = TRUE, test.always = TRUE))
write.csv(to_df(tab_s5), out_path("paper/suppl_table5_stacking.csv"), row.names = FALSE)
cat("\nStacking (endline, n households =", nrow(endline_stack), "):\n")
print(endline_stack |> group_by(study_arm) |> summarise(n = n(), lpg_primary = mean(primary_cooking_stove == "Gas stove", na.rm = TRUE),
                                                        any_other = mean(any_other_cooking_stove == "Yes", na.rm = TRUE)) |> as.data.frame())

# ── Table 1 (condensed) ───────────────────────────────────────────────────────
hh_t1 <- hh |> group_by(study_arm) |> summarise(
  households = n(),
  size_median = median(hh_mems_total), size_q1 = quantile(hh_mems_total, .25), size_q3 = quantile(hh_mems_total, .75),
  mother_children = sum(hh_arrangement == "Mother and children"), husband_wife_children = sum(hh_arrangement == "Husband and wife with their children"),
  poor_or_very_poor = sum(tolower(wealth) %in% c("poor", "very poor")), regular_charcoal_primary = sum(primary_cookstove == "Regular charcoal stove", na.rm = TRUE),
  gas_secondary = sum(secondary_cookstove == "Gas stove"), gas_any = sum(secondary_cookstove == "Gas stove" | tertiary_cookstove == "Gas stove"),
  stoves_median = median(total_cookstoves), .groups = "drop")
p_t1 <- p_data |> group_by(study_arm) |> summarise(
  participants = n(), age_median = median(age, na.rm = TRUE), age_q1 = quantile(age, .25, na.rm = TRUE), age_q3 = quantile(age, .75, na.rm = TRUE),
  female = sum(gen == "F"), educ_below_shs = sum(education == "< SHS"), not_working = sum(occupation == "Does not work"),
  trading = sum(occupation == "Trading"), primary_cook = sum(primary_cook == "Yes"), .groups = "drop")
pv <- function(tab) { s <- as.data.frame(tests(tab)); setNames(s$p.value, s$Variable) }
pvals <- c(pv(tab_hh), pv(tab_p))
t1 <- full_join(hh_t1, p_t1, by = "study_arm")
write.csv(t1, out_path("paper/table1_condensed_counts.csv"), row.names = FALSE)
write.csv(tibble(variable = names(pvals), p = round(pvals, 3)), out_path("paper/table1_pvalues.csv"), row.names = FALSE)
cat("\nTable 1 counts:\n"); print(as.data.frame(t1)); cat("\nP values:\n"); print(round(pvals, 3))
cat("\nDone.\n")
