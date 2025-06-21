# 01_preprocess_clean.R (v23 - Focus on accessing row 39)

# --- Log File Setup ---
log_file_path_01 <- "01_preprocess_clean_log.txt"
if(file.exists(log_file_path_01)) { try(file.remove(log_file_path_01), silent = TRUE) } # Ensure removal at start of script logic too
base_log_01 <- function(msg) {
  full_msg <- paste0(Sys.time(), ": ", msg, "\n")
  try(cat(full_msg, file = log_file_path_01, append = TRUE), silent = TRUE)
  cat(full_msg)
}

base_log_01("--- Script 01_preprocess_clean.R started (v23 - Focus access row 39) ---")
base_log_01(paste("R version:", R.version.string))
local_lib_path <- file.path(Sys.getenv("HOME"), "R", "library")
if (!dir.exists(local_lib_path)) { try(dir.create(local_lib_path, recursive = TRUE), silent = TRUE) }
.libPaths(c(local_lib_path, .libPaths()))
base_log_01(paste("Lib paths:", paste(.libPaths(), collapse=", ")))

# --- File Paths & Load RDS Data ---
processed_data_dir <- "data/processed"; cleaned_data_dir <- "data/cleaned"
rds_hist_file <- file.path(processed_data_dir, "loaded_historical_draws.rds")
rds_rel_file <- file.path(processed_data_dir, "loaded_relationship_df.rds")
base_log_01("Loading RDS files...")
if (!file.exists(rds_hist_file)) { base_log_01("CRIT: RDS hist missing"); stop("CRIT: RDS hist missing") }
if (!file.exists(rds_rel_file)) { base_log_01("CRIT: RDS rel missing"); stop("CRIT: RDS rel missing") }
historical_draws_df <- NULL; relationship_df <- NULL
tryCatch({ historical_draws_df <- readRDS(rds_hist_file) }, error = function(e) { base_log_01(paste0("ERROR reading hist RDS: ",conditionMessage(e))); stop(e) })
tryCatch({ relationship_df <- readRDS(rds_rel_file) }, error = function(e) { base_log_01(paste0("ERROR reading rel RDS: ", conditionMessage(e))); stop(e) })
base_log_01(paste("Loaded hist_df (",nrow(historical_draws_df),"x",ncol(historical_draws_df),"), rel_df (",nrow(relationship_df),"x",ncol(relationship_df),")"))

# --- 'Numbers' Parsing (v23) ---
base_log_01("Starting 'Numbers' column parsing (v23)...")
CRASH_FOCUS_ROW_V23 <- 39

if ("Numbers" %in% names(historical_draws_df) && is.character(historical_draws_df$Numbers)) {
  numbers_col_vector <- historical_draws_df$Numbers
  n_rows_total <- length(numbers_col_vector)
  base_log_01(paste("Total rows in 'Numbers' column:", n_rows_total))

  if (n_rows_total >= CRASH_FOCUS_ROW_V23) {
      base_log_01(paste0("Attempting to pre-access numbers_col_vector[", CRASH_FOCUS_ROW_V23, "] for inspection..."))
      focus_string_content_v23 <- "ACCESS_FAILED_OR_NA_PREINSPECT"
      focus_string_class_v23 <- "UNKNOWN_CLASS_PREINSPECT"
      tryCatch({
          focus_string_content_v23 <- numbers_col_vector[CRASH_FOCUS_ROW_V23]
          focus_string_class_v23 <- class(focus_string_content_v23)[1]
          base_log_01(paste0("Successfully pre-accessed string at focus row ", CRASH_FOCUS_ROW_V23,
                            " (class: ", focus_string_class_v23, "): '", substr(focus_string_content_v23,1,200),
                            ifelse(nchar(focus_string_content_v23)>200, "...'", "'")))
      }, error = function(e_preaccess) {
          base_log_01(paste0("ERROR during pre-access of numbers_col_vector[", CRASH_FOCUS_ROW_V23, "]: ", conditionMessage(e_preaccess)))
      })
  }

  historical_draws_df$Parsed_Numbers <- vector("list", n_rows_total)

  parse_num_string_v23 <- function(s_in, row_idx_func) {
    detailed_log_this_row <- (row_idx_func == CRASH_FOCUS_ROW_V23 || (row_idx_func >= CRASH_FOCUS_ROW_V23-1 && row_idx_func <= CRASH_FOCUS_ROW_V23+1) )
    general_log_this_iter <- (row_idx_func <= 3 || row_idx_func > (n_rows_total - 3) || row_idx_func %% 500 == 0 || row_idx_func == n_rows_total)

    if(detailed_log_this_row || general_log_this_iter) base_log_01(paste0("Parse_v23 ENTERED for row ", row_idx_func, ", input: '", substr(s_in,1,70),"...'"))

    result <- tryCatch({
        if(detailed_log_this_row) base_log_01(paste0("  Row ", row_idx_func, " step 1: is.na(s_in)"))
        if (is.na(s_in)) {
            if(detailed_log_this_row) base_log_01(paste0("  Row ", row_idx_func, " result: NA_real_ (input was NA)"))
            return(NA_real_)
        }
        if(detailed_log_this_row) base_log_01(paste0("  Row ", row_idx_func, " step 2: trimws(s_in). Input class: ", class(s_in)))
        s_trimmed_func <- trimws(s_in)
        if(detailed_log_this_row) base_log_01(paste0("    Row ", row_idx_func, " (s_trimmed_func): '", substr(s_trimmed_func,1,70), "'"))
        if (s_trimmed_func == "") {
            if(detailed_log_this_row) base_log_01(paste0("  Row ", row_idx_func, " result: NA_real_ (empty after trim)"))
            return(NA_real_)
        }
        if(detailed_log_this_row) base_log_01(paste0("  Row ", row_idx_func, " step 3: strsplit()"))
        num_strings_list_func <- strsplit(s_trimmed_func, ",", fixed = TRUE)
        num_strings_func <- num_strings_list_func[[1]]
        if(detailed_log_this_row) base_log_01(paste0("    Row ", row_idx_func, " (num_strings_func): '", paste(substr(num_strings_func,1,20),collapse="|"),"...'"))
        if(detailed_log_this_row) base_log_01(paste0("  Row ", row_idx_func, " step 4: filter empty/NA"))
        num_strings_filtered_func <- num_strings_func[num_strings_func != "" & !is.na(num_strings_func)]
        if(detailed_log_this_row) base_log_01(paste0("    Row ", row_idx_func, " (num_strings_filtered_func): '", paste(substr(num_strings_filtered_func,1,20),collapse="|"),"...'"))
        if(detailed_log_this_row) base_log_01(paste0("  Row ", row_idx_func, " step 5: as.numeric()"))
        as_numeric_final_result <- suppressWarnings(as.numeric(num_strings_filtered_func))
        if(detailed_log_this_row) base_log_01(paste0("    Row ", row_idx_func, " (as_numeric_final_result): '", paste(as_numeric_final_result,collapse="|"),"'"))
        if(all(is.na(as_numeric_final_result)) && !(is.na(s_in) || trimws(s_in) == "")){
            base_log_01(paste0("  Note (Parse_v23 row ", row_idx_func, "): '", substr(s_in,1,70),"...' -> all NAs."))
        }
        return(as_numeric_final_result)
    }, error = function(e) {
        base_log_01(paste0("    INNER PARSE ERROR row ", row_idx_func, ": '", substr(s_in,1,70), "'. Err: ", conditionMessage(e)))
        return(list("PARSING_ERROR_FLAG", conditionMessage(e)))
    })
    return(result)
  }

  base_log_01("Applying parse_num_string_v23 to all rows...")
  if (n_rows_total > 0) {
      for (i in 1:n_rows_total) {
          if(i == CRASH_FOCUS_ROW_V23) {
              base_log_01(paste0("LOOP: Pre-access log for FOCUS row ", i, ". About to access numbers_col_vector[", i, "]"))
          }
          current_string_to_parse <- numbers_col_vector[i]
          if(i == CRASH_FOCUS_ROW_V23) {
              base_log_01(paste0("LOOP: Post-access log for FOCUS row ", i, ". String: '", substr(current_string_to_parse,1,70), "'"))
              base_log_01(paste0("LOOP: About to call parse_num_string_v23 for FOCUS row ", i))
          }

          parsed_value <- parse_num_string_v23(current_string_to_parse, i)

          if(i == CRASH_FOCUS_ROW_V23) {
              base_log_01(paste0("LOOP: Returned from parse_num_string_v23 for FOCUS row ", i, ". Parsed value class: ", class(parsed_value)[1]))
              base_log_01(paste0("LOOP: About to assign to historical_draws_df$Parsed_Numbers[[",i,"]] for FOCUS row"))
          }
          historical_draws_df$Parsed_Numbers[[i]] <- parsed_value
          if(i == CRASH_FOCUS_ROW_V23) {
              base_log_01(paste0("LOOP: Assignment to historical_draws_df$Parsed_Numbers[[",i,"]] completed for FOCUS row."))
          }

          if (i %% 200 == 0 || i == n_rows_total || i <= 3 || i > (n_rows_total -3) ||
              (i >= CRASH_FOCUS_ROW_V23 -2 && i <= CRASH_FOCUS_ROW_V23 + 2) ) { # Adjusted logging range
              base_log_01(paste0("Parsing progress: Completed ", i, " of ", n_rows_total, " rows."))
          }
      }
  }
  base_log_01("'Parsed_Numbers' column processing complete.")
  error_flags_count <- 0; problematic_lengths_count <- 0; valid_vector_count <- 0; na_input_count <- 0
  if (n_rows_total > 0) {
    for(idx in 1:length(historical_draws_df$Parsed_Numbers)){
        item <- historical_draws_df$Parsed_Numbers[[idx]]
        if(is.list(item) && length(item)>0 && is.character(item[[1]]) && grepl("ERROR_FLAG", item[[1]][1])) {
            error_flags_count <- error_flags_count + 1
        } else if (length(item) == 1 && is.na(item[1])) {
            na_input_count <- na_input_count + 1
        } else if (length(item) == 5 && all(!is.na(item))) {
            valid_vector_count <- valid_vector_count + 1
        } else {
            problematic_lengths_count <- problematic_lengths_count + 1
            if(problematic_lengths_count <= 3) {
                base_log_01(paste0("Problematic item at index ", idx, ": Value: ", paste(unlist(item), collapse="|"), " Length: ", length(item)))
            }
        }
    }
  }
  base_log_01(paste0("Parsing validation summary: Total: ", n_rows_total, ", Valid: ", valid_vector_count,
                     ", NA_inputs: ", na_input_count, ", Errors: ", error_flags_count, ", OtherProbs: ", problematic_lengths_count))

} else { base_log_01("'Numbers' column not found/not character. Skipping parsing.") }
base_log_01("Numbers parsing section finished.")

base_log_01("Minimal preprocessing for relationship_df...")
if ("Number" %in% names(relationship_df) && !is.numeric(relationship_df$Number)) {
    relationship_df$Number <- suppressWarnings(as.numeric(as.character(relationship_df$Number)))
}
base_log_01("Minimal relationship_df preprocessing complete.")

base_log_01(paste("Attempting to save dataframes to:", cleaned_data_dir))
if (!dir.exists(cleaned_data_dir)) {
  try(dir.create(cleaned_data_dir, recursive = TRUE), silent = TRUE)
  if(!dir.exists(cleaned_data_dir)) {base_log_01("ERR: Failed to create cleaned_data_dir"); stop("failed create cleaned_data_dir")}
}
if(is.null(historical_draws_df)) { base_log_01("ERR: hist_df NULL"); stop("hist_df NULL") }
if(is.null(relationship_df)) { base_log_01("ERR: rel_df NULL"); stop("rel_df NULL") }
saveRDS(historical_draws_df, file.path(cleaned_data_dir, "cleaned_historical_draws.rds"))
saveRDS(relationship_df, file.path(cleaned_data_dir, "cleaned_relationship_df.rds"))
base_log_01("Saved dataframes to data/cleaned/.")

base_log_01("--- Script 01_preprocess_clean.R (v23 - Focus access row 39) finished ---")
cat("SCRIPT_COMPLETED_TOKEN_JULES_01_V23_ROW39ACCESS\n")
# End of script
