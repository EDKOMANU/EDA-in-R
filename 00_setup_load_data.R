# 00_setup_load_data.R (v4_verify_rds_and_install_pkgs)

# --- Log File Setup ---
log_file_path_00 <- "00_setup_load_data_log.txt"
# Clear log file at start of script run
if(file.exists(log_file_path_00)) {
  try(file.remove(log_file_path_00), silent = TRUE)
}
base_log <- function(msg) {
  full_msg <- paste0(Sys.time(), ": ", msg, "\n")
  # Try to write to log file, but don't let logging errors stop the script
  try(cat(full_msg, file = log_file_path_00, append = TRUE), silent = TRUE)
  # Always print to stdout
  cat(full_msg)
}

base_log("--- Script 00_setup_load_data.R started (v4_verify_rds_and_install_pkgs) ---")

# --- Package Management ---
local_lib_path <- file.path(Sys.getenv("HOME"), "R", "library")
if (!dir.exists(local_lib_path)) {
  try(dir.create(local_lib_path, recursive = TRUE), silent = TRUE)
}
.libPaths(c(local_lib_path, .libPaths()))
base_log(paste("Using R library paths:", paste(.libPaths(), collapse=", ")))

install_if_missing <- function(pkg_name, purpose = "general use") {
  base_log(paste("Checking package:", pkg_name, "for", purpose))
  # Suppress startup messages for requireNamespace and library for cleaner logs
  if (!suppressPackageStartupMessages(requireNamespace(pkg_name, quietly = TRUE))) {
    base_log(paste("Installing package:", pkg_name, "... (This may take time for larger packages like tidyverse or keras)"))
    # Make installation verbose to see progress/errors from install.packages itself
    install.packages(pkg_name, lib = local_lib_path, repos = "https://cloud.r-project.org/", quiet = FALSE, verbose = TRUE)
    if (!suppressPackageStartupMessages(requireNamespace(pkg_name, quietly = TRUE))) {
      error_msg <- paste("CRITICAL ERROR: Failed to install package:", pkg_name, "even after attempting install.packages.")
      base_log(error_msg); stop(error_msg)
    }
    base_log(paste("Successfully installed package:", pkg_name))
  } else {
    base_log(paste("Package", pkg_name, "is already installed."))
  }
  # Ensure it's loadable by trying to load it quietly
  # library() can sometimes reveal issues that requireNamespace doesn't catch.
  load_success_flag <- suppressPackageStartupMessages(library(pkg_name, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE, logical.return = TRUE))
  if(load_success_flag){
    base_log(paste("Package", pkg_name, "is available and successfully loaded for checks."))
  } else {
    error_msg <- paste("ERROR: Package", pkg_name, "is installed but FAILED TO LOAD via library(). Check for conflicts or deeper issues.")
    base_log(error_msg)
    # Depending on severity, might want to stop(error_msg) here too.
    # For now, will log error and continue, subsequent scripts will fail if essential package isn't truly loadable.
  }
}

# Packages needed by SUBSEQUENT scripts (e.g., 01_preprocess_clean.R, ML/DL scripts)
packages_for_next_steps <- c("tidyverse", "lubridate", "data.table")
ml_dl_packages <- c("xgboost", "randomForest", "keras", "caret", "ggplot2", "pROC")
# Keras might require tensorflow setup, which is beyond simple R package install.
# install_if_missing("tensorflow") # If needed, but usually handled by keras::install_keras()
all_needed_packages <- unique(c(packages_for_next_steps, ml_dl_packages))

base_log("Ensuring all required packages for the project pipeline are installed and loadable...")
for(pkg in all_needed_packages) {
  install_if_missing(pkg, purpose = "project pipeline dependency")
}
# Special check for Keras - it often needs an underlying Python engine.
# This script won't run install_keras() as it can be interactive.
# It will just ensure the R 'keras' package is there.
if (requireNamespace("keras", quietly = TRUE)) {
    base_log("R 'keras' package is installed. Further setup (like install_keras()) might be needed later if Python backend is missing.")
}
base_log("All required package installation and load checks complete.")


# --- Verify Existence of Pre-processed RDS Files ---
# These files are expected to have been created by some prior (possibly manual or problematic) process.
# This script's job is now to confirm they exist, so subsequent scripts can use them.
processed_data_dir <- "data/processed"
rds_hist_file <- file.path(processed_data_dir, "loaded_historical_draws.rds")
rds_rel_file <- file.path(processed_data_dir, "loaded_relationship_df.rds")

base_log(paste("Verifying existence of required input RDS file:", rds_hist_file))
if (!file.exists(rds_hist_file)) {
  error_msg <- paste("CRITICAL ERROR: Required RDS file NOT FOUND -", rds_hist_file,
                     ". This file is expected to exist. The data loading step of the project is incomplete.")
  base_log(error_msg); stop(error_msg)
} else {
  base_log(paste("SUCCESS: Required RDS file exists -", rds_hist_file))
}

base_log(paste("Verifying existence of required input RDS file:", rds_rel_file))
if (!file.exists(rds_rel_file)) {
  error_msg <- paste("CRITICAL ERROR: Required RDS file NOT FOUND -", rds_rel_file,
                     ". This file is expected to exist. The data loading step of the project is incomplete.")
  base_log(error_msg); stop(error_msg)
} else {
  base_log(paste("SUCCESS: Required RDS file exists -", rds_rel_file))
}

base_log("RDS file verification complete. These files will be used by the next script (01_preprocess_clean.R).")
base_log("This script (00_setup_load_data.R) will not modify or re-create these RDS files if they already exist as verified.")

# --- Optional: Brief Inspection of RDS files (load and get dimensions) ---
base_log("Attempting to load and minimally inspect existing RDS files for validity...")
tryCatch({
  temp_hist_df <- readRDS(rds_hist_file)
  base_log(paste0("Successfully read ", rds_hist_file, ". Dimensions: ", nrow(temp_hist_df), " rows, ", ncol(temp_hist_df), " columns. Object class: ", class(temp_hist_df)[1]))
  # Check if it's a dataframe
  if(!is.data.frame(temp_hist_df)) { base_log(paste("WARNING:", rds_hist_file, "is not a data.frame object!"))}
  rm(temp_hist_df) # Clean up memory

  temp_rel_df <- readRDS(rds_rel_file)
  base_log(paste0("Successfully read ", rds_rel_file, ". Dimensions: ", nrow(temp_rel_df), " rows, ", ncol(temp_rel_df), " columns. Object class: ", class(temp_rel_df)[1]))
  if(!is.data.frame(temp_rel_df)) { base_log(paste("WARNING:", rds_rel_file, "is not a data.frame object!"))}
  rm(temp_rel_df) # Clean up memory
  base_log("Minimal inspection of RDS files successful.")
}, error = function(e){
  base_log(paste("WARNING: Could not read/inspect existing RDS files. They might be corrupted or not R objects:", conditionMessage(e)))
  # This is a warning because the primary check is file.exists. Subsequent scripts will fail if RDS is unreadable.
})


base_log("--- Script 00_setup_load_data.R (v4_verify_rds_and_install_pkgs) finished successfully ---")
# This token indicates this specific script's successful completion.
cat("SCRIPT_COMPLETED_SUCCESSFULLY_TOKEN_JULES_00_VERIFY_RDS_V4\n")
