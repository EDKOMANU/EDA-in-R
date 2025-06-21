# install_openxlsx_only.R

log_file <- "install_openxlsx_log.txt"
if(file.exists(log_file)) try(file.remove(log_file), silent = TRUE) # Clear previous log

base_log_install <- function(msg) {
  full_msg <- paste0(Sys.time(), ": ", msg, "\n")
  # Try to write to log file, but don't let logging errors stop the script
  try(cat(full_msg, file = log_file, append = TRUE), silent = TRUE)
  # Always print to stdout
  cat(full_msg)
}

base_log_install("--- Starting install_openxlsx_only.R ---")

local_lib_path <- file.path(Sys.getenv("HOME"), "R", "library")
if (!dir.exists(local_lib_path)) {
  try(dir.create(local_lib_path, recursive = TRUE), silent = TRUE)
}
.libPaths(c(local_lib_path, .libPaths())) # Add local lib path to the front
base_log_install(paste("Using R library paths:", paste(.libPaths(), collapse=", ")))

# Install 'zip' R package first, as openxlsx often depends on it
base_log_install("Checking/Installing R package 'zip'...")
if (!requireNamespace("zip", quietly = TRUE)) {
  base_log_install("R package 'zip' not found by requireNamespace. Attempting installation...")
  install.packages("zip", lib = local_lib_path, repos = "https://cloud.r-project.org/", quiet = FALSE, verbose = TRUE)
  # Re-check after install attempt
  if (requireNamespace("zip", quietly = TRUE)) {
    base_log_install("R package 'zip' installed successfully.")
  } else {
    base_log_install("ERROR: Failed to install R package 'zip'. This might affect 'openxlsx'.")
  }
} else {
  base_log_install("R package 'zip' is already available.")
}

# Now, check/install 'openxlsx'
base_log_install("Checking/Installing R package 'openxlsx'...")
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  base_log_install("R package 'openxlsx' not found by requireNamespace. Attempting installation... (This can take some time)")
  install.packages("openxlsx", lib = local_lib_path, repos = "https://cloud.r-project.org/", quiet = FALSE, verbose = TRUE) # Verbose install
  # Re-check after install attempt
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    base_log_install("R package 'openxlsx' installed successfully.")
  } else {
    base_log_install("ERROR: Failed to install R package 'openxlsx'.")
    # stop("Stopping script as openxlsx installation failed.") # Option to stop
  }
} else {
  base_log_install("R package 'openxlsx' is already available.")
}

# Try loading openxlsx to confirm it's usable
base_log_install("Attempting to load 'openxlsx' via library()...")
load_success <- FALSE
tryCatch({
  # logical.return=TRUE makes library() return TRUE/FALSE instead of erroring on fail with some configurations
  load_success <- suppressPackageStartupMessages(library("openxlsx", character.only = TRUE, logical.return = TRUE, warn.conflicts = FALSE))
}, error = function(e) {
  base_log_install(paste("Error during library('openxlsx'):", conditionMessage(e)))
})

if (load_success) {
    base_log_install("'openxlsx' loaded successfully via library().")
} else {
    base_log_install("ERROR: Failed to load 'openxlsx' via library() even if installation was reported as successful.")
}

base_log_install("--- Finished install_openxlsx_only.R ---")
