# ==============================================================================
# config.R — the only file to edit before running the pipeline.
# Set the two folders below (absolute paths, or paths relative to this folder).
# ==============================================================================
DATA_DIR   <- "PATH/TO/DATA"     # folder holding the study data files listed in the README
OUTPUT_DIR <- "PATH/TO/OUTPUT"   # folder where tables, figures and logs are written (created if missing)

data_path <- function(...) file.path(DATA_DIR, ...)
out_path  <- function(...) file.path(OUTPUT_DIR, ...)
if (DATA_DIR == "PATH/TO/DATA" || OUTPUT_DIR == "PATH/TO/OUTPUT")
  stop("Edit config.R: set DATA_DIR and OUTPUT_DIR before running the pipeline.")
if (!dir.exists(DATA_DIR)) stop("DATA_DIR does not exist: ", DATA_DIR)
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
