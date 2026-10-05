# ==============================================================================
# 01_sums_events.R
# Stove-use monitor (SUMS) processing: detect cooking events on every stove
# temperature logger with the FireFinder algorithm (sumsarizer), using the
# stove-specific thresholds established in the study team's SUMS cleaning (thresholds below; see README).
#
# Raw loggers: Lascar EL-USB-TC thermocouple loggers (.dlg, binary) in
#   <DATA_DIR>/complete_data/sums_data/.  They were exported to CSV with the Lascar
#   EasyLog software (UTF-16 CSV: Index, Timestamp, Thermocouple(°C)) into
#   <DATA_DIR>/complete_data/sums_data_csv/converted SUM csv/ (812 files).
#
# Run from this folder (after editing config.R):  Rscript 01_sums_events.R   (about 1 min for 812 files)
# Outputs: <DATA_DIR>/processed_data/sums/sums_events_all.csv     one row per event
#          <DATA_DIR>/processed_data/sums/sums_file_inventory.csv one row per logger file
# ==============================================================================
source("config.R")   # DATA_DIR, OUTPUT_DIR, data_path(), out_path() — edit config.R before running
suppressPackageStartupMessages({ library(tidyverse); library(lubridate); library(sumsarizer); library(tools) })
csv_dir <- data_path("complete_data/sums_data_csv/converted SUM csv")
out_dir <- data_path("processed_data/sums"); dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
t0 <- Sys.time()

# ── FireFinder thresholds (°C), minimum event length (s), minimum gap (s) ─────
#   lpg 45: LPG transfers little heat to a sensor on the stove body (sumsarizer vignette)
#   coalpot 55: regular coalpot sensors reach 90-180 °C; 55 clears ambient (~28-35 °C)
#   improvecoalpot 38: improved coalpots in this deployment peaked at 45-51 °C
#                      (sensor on the insulated body) — lower-confidence detections
#   threestone / firewood 75: sumsarizer default for open biomass fires
THRESHOLDS <- list(
  lpg            = list(primary_threshold = 45, min_event_sec = 600, min_break_sec = 1800),
  coalpot        = list(primary_threshold = 55, min_event_sec = 600, min_break_sec = 1800),
  improvecoalpot = list(primary_threshold = 38, min_event_sec = 600, min_break_sec = 1800),
  threestone     = list(primary_threshold = 75, min_event_sec = 600, min_break_sec = 1800),
  firewood       = list(primary_threshold = 75, min_event_sec = 600, min_break_sec = 1800)
)
BAD_SENSOR_PCT <- 50; PLAUSIBLE <- c(-10, 800)

# ── Filename metadata: SUMS<num>_<YYYYMMDD>_<HHID>_<STOVE>[_copyN].csv ────────
EXCEPTIONS <- list(
  "SUMS083_20221111_LPGT2"           = list(sum_number = "083", household_id = NA, stove_type_raw = "LPGT2"),
  "SUMS089_20221122_REGULARCOALPOT"  = list(sum_number = "089", household_id = NA, stove_type_raw = "REGULARCOALPOT"),
  "SUMS098_20221108_REGULARCOALPOT"  = list(sum_number = "098", household_id = NA, stove_type_raw = "REGULARCOALPOT"),
  "SUMS154_20221028_IMPROVECOALPOT"  = list(sum_number = "154", household_id = NA, stove_type_raw = "IMPROVECOALPOT"),
  "SUMS187_20221104_REGULARCOALPOT"  = list(sum_number = "187", household_id = NA, stove_type_raw = "REGULARCOALPOT"),
  "SUMS319_20220916_REGULARCOALPOT1" = list(sum_number = "319", household_id = NA, stove_type_raw = "REGULARCOALPOT1"),
  "SUMS320_20220916_IMPROVEFIREWOOD" = list(sum_number = "320", household_id = NA, stove_type_raw = "IMPROVEFIREWOOD"),
  "SUMS330_20221110_LPG2"            = list(sum_number = "330", household_id = NA, stove_type_raw = "LPG2"),
  "SUMS358_20221125_LPGT1"           = list(sum_number = "358", household_id = NA, stove_type_raw = "LPGT1"),
  "SUMS191_20221206_E"               = list(sum_number = "191", household_id = NA, stove_type_raw = "UNKNOWN"),
  "SUMS337_20221117_E047M"           = list(sum_number = "337", household_id = "E047", stove_type_raw = "UNKNOWN"),
  "SUMS368_20221128_E096M"           = list(sum_number = "368", household_id = "E096", stove_type_raw = "UNKNOWN"),
  "SUMS_113_20220901_E157_LPG"       = list(sum_number = "113", household_id = "E157", stove_type_raw = "LPG"),
  "SUMS_128_20220901_E157_CAOLPOT"   = list(sum_number = "128", household_id = "E157", stove_type_raw = "CAOLPOT"),
  "SUMS_130_20220901_E031_COALPOT1"  = list(sum_number = "130", household_id = "E031", stove_type_raw = "COALPOT1"),
  "SUMS_138_20220908_E198_THREESTONE" = list(sum_number = "138", household_id = "E198", stove_type_raw = "THREESTONE"),
  "SUMS_147_20220909_E191_IMPROVE_COALPOT" = list(sum_number = "147", household_id = "E191", stove_type_raw = "IMPROVECOALPOT"),
  "SUMS_150_20220909_E191_LPG"       = list(sum_number = "150", household_id = "E191", stove_type_raw = "LPG"),
  "SUMS_152_20220908_E027_IMPROVEFIREWOOD" = list(sum_number = "152", household_id = "E027", stove_type_raw = "IMPROVEFIREWOOD"),
  "SUMS_160_20220908_E198_COALPOT"   = list(sum_number = "160", household_id = "E198", stove_type_raw = "COALPOT"),
  "SUMS_165_20220908_E192_THREESTONE" = list(sum_number = "165", household_id = "E192", stove_type_raw = "THREESTONE"),
  "SUMS_166_20220909_E134_LPG"       = list(sum_number = "166", household_id = "E134", stove_type_raw = "LPG"),
  "SUMS_168_20220909_E130_COALPOT"   = list(sum_number = "168", household_id = "E130", stove_type_raw = "COALPOT"),
  "SUMS_172_20220908_E175_COALPOT"   = list(sum_number = "172", household_id = "E175", stove_type_raw = "COALPOT"),
  "SUMS_174_20220908_E192_IMPROVECOALPOT" = list(sum_number = "174", household_id = "E192", stove_type_raw = "IMPROVECOALPOT"),
  "SUMS_175_20220908_E027_REGULARCOALPOT" = list(sum_number = "175", household_id = "E027", stove_type_raw = "REGULARCOALPOT"),
  "SUMS_180_20220909_E200_IMPROVE_COALPOAT1" = list(sum_number = "180", household_id = "E200", stove_type_raw = "IMPROVECOALPOT"),
  "SUMS_182_20220909_E130_3STONE"    = list(sum_number = "182", household_id = "E130", stove_type_raw = "3STONE"),
  "SUMS_183_20220909_E153_IMPROVE_COALPOT" = list(sum_number = "183", household_id = "E153", stove_type_raw = "IMPROVECOALPOT"),
  "SUMS_292_20220909_E134_LPG"       = list(sum_number = "292", household_id = "E134", stove_type_raw = "LPG"),
  "SUMS_295_20220909_E200_IMPROVE_COALPOT2" = list(sum_number = "295", household_id = "E200", stove_type_raw = "IMPROVECOALPOT"),
  "SUMS_300_20220909_E130_IMPROVE_COALPOT" = list(sum_number = "300", household_id = "E130", stove_type_raw = "IMPROVECOALPOT"),
  "SUMS_304_20220909_E191_LPG"       = list(sum_number = "304", household_id = "E191", stove_type_raw = "LPG"),
  "SUMS_308_20220909_E134_COALPOT"   = list(sum_number = "308", household_id = "E134", stove_type_raw = "COALPOT"),
  "SUMS166_20221125_E134_20221125_LPG1" = list(sum_number = "166", household_id = "E134", stove_type_raw = "LPG")
)
extract_metadata <- function(path) {
  base <- file_path_sans_ext(basename(path))
  key  <- gsub("_copy[0-9]*$", "", base); key <- gsub("[+-]", "_", key)
  if (key %in% names(EXCEPTIONS)) { ex <- EXCEPTIONS[[key]]
    return(tibble(file = base, sum_number = ex$sum_number, household_id = ex$household_id, stove_type_raw = ex$stove_type_raw)) }
  parts <- strsplit(key, "_")[[1]]; parts <- parts[parts != ""]
  if (length(parts) >= 4) tibble(file = base, sum_number = gsub("^SUMS?", "", parts[1]),
                                 household_id = ifelse(grepl("^E[0-9]{3}", parts[3]), substr(parts[3], 1, 4), NA),
                                 stove_type_raw = gsub("\\.CSV$", "", paste(parts[4:length(parts)], collapse = "_"), ignore.case = TRUE))
  else tibble(file = base, sum_number = NA, household_id = NA, stove_type_raw = NA)
}
standardize_stove_type <- function(raw) {
  r <- toupper(coalesce(raw, ""))
  case_when(str_detect(r, "LPGT|LPFT|^LPG") ~ "lpg",
            str_detect(r, "IMPROVE.*COAL|IMPROVECOAL|IMPROVE_COAL|CAOLP|COALPOAT") ~ "improvecoalpot",
            str_detect(r, "COAL|COALPOT|COAOLP|COA0|C0ALPOT") ~ "coalpot",
            str_detect(r, "THREESTONE|3STONE|THRESTONE|STONE") ~ "threestone",
            str_detect(r, "FIREWOOD") ~ "firewood",
            TRUE ~ "unknown")
}

# ── Loop over files ───────────────────────────────────────────────────────────
files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)
is_copy <- grepl("_copy[0-9]*\\.csv$", files, ignore.case = TRUE)
cat("CSV files:", length(files), " copies excluded:", sum(is_copy), "\n")
files <- files[!is_copy]
inventory <- list(); events <- list()
for (i in seq_along(files)) {
  f <- files[i]; meta <- extract_metadata(f); st <- standardize_stove_type(meta$stove_type_raw)
  row <- meta |> mutate(stove_type = st, status = NA_character_, n_readings = NA_integer_, interval_min = NA_real_,
                        first_ts = as.POSIXct(NA), last_ts = as.POSIXct(NA), pct_implausible = NA_real_, n_events = NA_integer_)
  res <- tryCatch({
    if (st == "unknown") { row$status <- "unknown_stove"; row }
    else {
      d <- suppressWarnings(import_sums(f))
      if (is.null(d) || nrow(d) == 0) { row$status <- "empty"; row }
      else {
        v <- d$value[!is.na(d$value)]
        row$n_readings <- nrow(d); row$first_ts <- min(d$timestamp, na.rm = TRUE); row$last_ts <- max(d$timestamp, na.rm = TRUE)
        row$interval_min <- as.numeric(median(diff(as.numeric(d$timestamp)), na.rm = TRUE)) / 60
        row$pct_implausible <- if (length(v)) 100 * mean(v < PLAUSIBLE[1] | v > PLAUSIBLE[2]) else 100
        if (row$pct_implausible >= BAD_SENSOR_PCT) { row$status <- "bad_sensor"; row }
        else {
          p  <- THRESHOLDS[[st]]
          ev <- suppressWarnings(list_events(apply_detector(d, firefinder_detector, primary_threshold = p$primary_threshold,
                                                            min_event_sec = p$min_event_sec, min_break_sec = p$min_break_sec)))
          ev <- as.data.frame(ev)
          row$status <- "processed"; row$n_events <- nrow(ev)
          if (nrow(ev) > 0) events[[length(events) + 1]] <- ev |>
            transmute(file = meta$file, sum_number = meta$sum_number, household_id = meta$household_id, stove_type = st,
                      stove_type_raw = meta$stove_type_raw, threshold_c = p$primary_threshold, event_num,
                      start_time, stop_time, duration_min = duration_mins, min_temp, max_temp)
          row
        }
      }
    }
  }, error = function(e) { row$status <- paste("error:", conditionMessage(e)); row })
  inventory[[i]] <- res
  if (i %% 50 == 0) cat(sprintf("  %d/%d files  (%.1f min)\n", i, length(files), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
inv <- bind_rows(inventory); evs <- bind_rows(events)
write.csv(inv, file.path(out_dir, "sums_file_inventory.csv"), row.names = FALSE)
write.csv(evs, file.path(out_dir, "sums_events_all.csv"), row.names = FALSE)
cat("\nStatus counts:\n"); print(table(inv$status))
cat("\nEvents detected:", nrow(evs), " files with events:", n_distinct(evs$file),
    " households:", n_distinct(evs$household_id), "\n")
print(evs |> count(stove_type))
cat("Done in", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "min\n")
