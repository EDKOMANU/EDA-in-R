# main.R

# Install readxl if not already installed
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

# Load libraries
library(tidyverse)
library(readxl)

# --- Project Setup and Data Loading ---
setwd("~/GitHub/EDA-in-R")
# Define file paths
historical_draws_file <- "data/historical_draws.xlsx"
relationship_table_file <- "data/relationship_table.xlsx"

# Load the datasets
historical_draws_df <- read_excel(historical_draws_file)
relationship_df <- read_excel(relationship_table_file)

# Initial inspection of the data
print("--- Historical Draws Data ---")
print(str(historical_draws_df))
print(head(historical_draws_df))
# summary(historical_draws_df) # Optional: for more detailed summary

print("--- Number Relationship Data ---")
print(str(relationship_df))
print(head(relationship_df))
# summary(relationship_df) # Optional: for more detailed summary

print("Data loading complete. Next steps: Preprocessing and Cleaning...")

# --- Data Preprocessing and Cleaning ---

# Load lubridate for easier date manipulation (though not strictly needed for POSIXct yet)
if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}
library(lubridate)

# 1. Preprocessing Historical Draws (historical_draws_df)

print("--- Preprocessing Historical Draws Data ---")

# Parse 'Numbers' column
# Option 1: Using tidyr::separate_rows and then grouping if needed later
historical_draws_df <- historical_draws_df |>
  mutate(Numbers_List = strsplit(as.character(Numbers), ",")) |>
  mutate(Numbers_List = lapply(Numbers_List, as.numeric))

# Check for NAs introduced by as.numeric (if any non-numeric parts were in Numbers string)
if (any(sapply(historical_draws_df$Numbers_List, function(x) any(is.na(x))))) {
  print("Warning: NAs introduced during Numbers parsing. Review problematic rows.")
  # To see problematic rows:
  # historical_draws_df |> filter(sapply(Numbers_List, function(x) any(is.na(x)))) |> print()
}

# Check for missing values in key columns
print(paste("Missing values in DrawID:", sum(is.na(historical_draws_df$DrawID))))
print(paste("Missing values in Date:", sum(is.na(historical_draws_df$Date))))
print(paste("Missing values in Draw:", sum(is.na(historical_draws_df$Draw))))
print(paste("Missing values in original Numbers string:", sum(is.na(historical_draws_df$Numbers))))
# For Numbers_List, an empty list or list of NAs would be an issue.
# We can check if any list is empty or only contains NAs
print(paste("Draws with issues in parsed Numbers_List:", sum(sapply(historical_draws_df$Numbers_List, function(x) all(is.na(x)) | length(x) == 0))))

# Verify DrawID uniqueness
if (any(duplicated(historical_draws_df$DrawID))) {
  print("Warning: Duplicate DrawIDs found. Creating a new unique_DrawID based on row number for now.")
  # To see problematic rows:
  # print(historical_draws_df[duplicated(historical_draws_df$DrawID) | duplicated(historical_draws_df$DrawID, fromLast = TRUE), ])
  historical_draws_df <- historical_draws_df |>
    mutate(unique_DrawID = row_number())
  print("Added unique_DrawID column.")
} else {
  print("DrawID is unique. Copying to unique_DrawID.")
  historical_draws_df <- historical_draws_df |>
    mutate(unique_DrawID = DrawID)
}

print("Historical Draws Data - Processed Structure:")
print(str(historical_draws_df))
print(head(historical_draws_df))


# 2. Preprocessing Number Relationship Table (relationship_df)
print("--- Preprocessing Number Relationship Data ---")

# Rename 'String Key'
if ("String Key" %in% names(relationship_df)) {
  relationship_df <- relationship_df |>
    rename(String_Key = `String Key`)
  print("Renamed 'String Key' to 'String_Key'")
}

# Check if all numbers from 1 to 90 are present and unique in 'Number' column
expected_numbers <- 1:90
if (all(expected_numbers %in% relationship_df$Number) && 
    length(unique(relationship_df$Number)) == 90 &&
    nrow(relationship_df) == 90) {
  print("All numbers from 1 to 90 are present and unique in relationship_df$Number.")
} else {
  print("Warning: Issue with 'Number' column in relationship_df. Not all numbers 1-90 present/unique or row count mismatch.")
  print(paste("Unique numbers found:", length(unique(relationship_df$Number))))
  print(paste("Missing numbers:", paste(setdiff(expected_numbers, relationship_df$Number), collapse = ", ")))
}

# Investigate and handle 0 values in relationship columns
# For now, let's check which columns contain 0. Lottery numbers are 1-90.
# A 0 might mean 'no relationship' or be an error.
# We will convert 0 to NA in relationship columns if they are intended to hold lottery numbers.
relationship_cols_to_check <- c("Bonanza", "Counterparts", "Malta", "String_Key", 
                                "shadow", "Partner", "Equivalent", "Code", "Turning")

for (col_name in relationship_cols_to_check) {
  if (col_name %in% names(relationship_df)) {
    if (any(relationship_df[[col_name]] == 0, na.rm = TRUE)) {
      print(paste("Found 0 in column:", col_name, ". Count:", sum(relationship_df[[col_name]] == 0, na.rm = TRUE)))
      # Assuming 0 means 'no such related number' or is out of 1-90 range for lottery numbers.
      # For now, let's convert to NA. This can be revisited if 0 has a specific meaning.
      # 'Code' might be an exception if it's not a lottery number itself.
      if (col_name != "Code") { # Example: 'Code' might be allowed to be 0.
        relationship_df <- relationship_df |>
          mutate(!!sym(col_name) := na_if(!!sym(col_name), 0))
        print(paste("Converted 0s to NAs in column:", col_name))
      }
    }
    # Also check for values outside 1-90 range if they are supposed to be lottery numbers
    # For 'Code', 'shadow' (if 0 is allowed), this check might be different.
    # This is a simple check, more sophisticated validation might be needed.
    if (col_name != "Code"){ # Assuming 'Code' is not a lottery number from 1-90
      # Identify numbers outside the 1-90 range (excluding NAs)
      invalid_mask <- !is.na(relationship_df[[col_name]]) & (relationship_df[[col_name]] < 1 | relationship_df[[col_name]] > 90)
      invalid_numbers <- relationship_df[[col_name]][invalid_mask]
      
      if(length(invalid_numbers) > 0){
        print(paste("Warning: Found numbers outside 1-90 range in", col_name, ":", paste(unique(invalid_numbers), collapse=", ")))
        # Convert these out-of-range numbers to NA
        relationship_df <- relationship_df |>
          mutate(!!sym(col_name) := ifelse(invalid_mask, NA, !!sym(col_name)))
        print(paste("Converted out-of-range numbers to NAs in column:", col_name))
      }
    }
  }
}


# Check for missing values in all relationship columns
print("Missing values per column in relationship_df after processing 0s:")
print(colSums(is.na(relationship_df)))

print("Number Relationship Data - Processed Structure:")
print(str(relationship_df))
print(head(relationship_df))

# Placeholder for next steps
print("Data preprocessing and cleaning complete. Next steps: Feature Engineering...")

# --- Feature Engineering - Historical Draws ---
print("--- Feature Engineering - Historical Draws ---")

# Ensure lubridate is loaded (it should be from previous step)
# library(lubridate) # Already loaded
# library(dplyr) # Already loaded
# library(tidyr) # Already loaded

# 1. Transform Data to Long Format
# Unnest the Numbers_List to have one row per drawn number per draw
# Keep unique_DrawID, Date, Draw type
historical_draws_long_df <- historical_draws_df |>
  select(unique_DrawID, Date, Draw, Numbers_List) |>
  unnest(Numbers_List) |>
  rename(Drawn_Number = Numbers_List) |>
  arrange(Date, unique_DrawID, Drawn_Number) # Ensure chronological order for calculations

print("Historical Draws Long Format - Structure:")
print(str(historical_draws_long_df))
print(head(historical_draws_long_df))

# --- Calculate Frequency ---
# Overall frequency of each number (how many times it has appeared in total)
# This is not a 'feature for a specific draw' yet, but a global stat that can be used later.
number_total_frequency_df <- historical_draws_long_df |>
  group_by(Drawn_Number) |>
  summarise(Total_Appearances = n(), .groups = 'drop') |>
  arrange(Drawn_Number)

print("Overall Frequency of each number (global statistic):")
print(head(number_total_frequency_df))

# --- Calculate Recency (days_since_last_drawn and draws_since_last_drawn) ---
# This needs to be calculated per number.
# We'll group by Drawn_Number and then calculate the difference in days and draws.

# Create a helper df with draw dates and a sequential draw number
# This ensures Draw_Seq_No is based on the actual draw chronology, not just unique_DrawID if it had gaps or was just row_number
# For this, we need to consider unique draws from the original historical_draws_df
# Sort historical_draws_df by Date and then by unique_DrawID to ensure consistent sequencing
# In case of draws on the same date (e.g. different lottery types), unique_DrawID helps maintain order.
# If DrawID itself was reliable and sequential for unique draws, it could be used.
# Given duplicate DrawIDs, we rely on Date and the unique_DrawID (row_number from original).

draw_sequence_df <- historical_draws_df |>
  select(unique_DrawID, Date) |>
  distinct() |> # Get unique draw instances
  arrange(Date, unique_DrawID) |> # Order them chronologically
  mutate(Draw_Seq_No = row_number()) # Assign a sequential draw number

# Join this sequence number back to the long dataframe
historical_draws_long_df <- historical_draws_long_df |>
  left_join(draw_sequence_df, by = c("unique_DrawID", "Date")) |>
  arrange(Drawn_Number, Date, unique_DrawID) # Critical: sort by number, then by time for lag calculation

# Calculate days_since_last_drawn and draws_since_last_drawn (which is the draw interval)
historical_draws_long_df <- historical_draws_long_df |>
  group_by(Drawn_Number) |>
  mutate(
    Last_Draw_Date = lag(Date, 1),
    Last_Draw_Seq_No = lag(Draw_Seq_No, 1)
  ) |>
  ungroup() |> # Ungroup before next mutate to avoid issues with non-grouped variables
  mutate(
    Days_Since_Last_Drawn = if_else(!is.na(Last_Draw_Date), as.numeric(difftime(Date, Last_Draw_Date, units = "days")), NA_real_),
    Draws_Since_Last_Drawn = if_else(!is.na(Last_Draw_Seq_No), Draw_Seq_No - Last_Draw_Seq_No, NA_integer_)
  ) |>
  select(-Last_Draw_Date, -Last_Draw_Seq_No) # Clean up temporary lag columns

print("Historical Draws Long Format with Recency/Interval - Structure & Sample:")
print(str(historical_draws_long_df))
# Print a sample for a specific number to see the effect
print("Sample for Drawn_Number == 1 (to check recency/interval):")
print(head(historical_draws_long_df |> filter(Drawn_Number == 1), 10))


# Note on Rolling Frequencies and Lag Features for "all potential numbers":
# The features above (Days_Since_Last_Drawn, Draws_Since_Last_Drawn) are calculated *for each appearance* of a number.
# True rolling frequencies (e.g. "how many times number X appeared in the last 50 calendar draws") or
# lag features (e.g. "was number X drawn in the immediately preceding calendar draw?")
# for *all 90 numbers for each draw* typically require a different data structure setup:
# One row per draw, with columns for each number (0/1 if drawn or not), or one row per (draw, potential_number) pair.
# This transformation is usually done in the "Data Transformation for Modeling" step.
# The current `historical_draws_long_df` and `number_total_frequency_df` are valuable inputs for that step.

# For now, the key historical features derived are:
# 1. `historical_draws_long_df`: Contains each instance a number was drawn, along with its `Date`, `Draw_Seq_No`,
#    `Days_Since_Last_Drawn` (since its own last appearance), and `Draws_Since_Last_Drawn` (draw interval for its reappearances).
# 2. `number_total_frequency_df`: Global total appearances for each number.

# These will be used to construct features for the modeling dataset later.
# For example, when predicting for Draw D, for each number 1-90, we can look up its last appearance date from historical_draws_long_df
# to calculate its current recency, or use its total frequency.

print("Feature engineering from historical draws (initial set for long format) complete.")

# --- Feature Engineering - Relationship Table & Combined Features ---
print("--- Feature Engineering - Relationship Table & Combined Features ---")

# Relationship columns to use for feature generation
relationship_types <- c("Bonanza", "Counterparts", "Malta", "String_Key", 
                        "shadow", "Partner", "Equivalent", "Turning")

# Make sure relationship_df 'Number' column is suitable for joining and has no NA numbers
# (already preprocessed, but good to be mindful)
relationship_df_fe <- relationship_df |> rename(Ref_Number = Number)

# Create a list of drawn numbers for each draw_seq_no for easier lookup
# This will be our S_current, S_previous_1 etc.
# Ensure unique_DrawID is the primary key for draws here.
draw_sets <- historical_draws_long_df |>
  group_by(unique_DrawID, Draw_Seq_No) |> # Group by both to maintain sequence integrity
  summarise(S_current = list(unique(Drawn_Number)), .groups = 'drop') |>
  arrange(Draw_Seq_No)


# --- Relationship Features: Co-occurrence in CURRENT Draw ---
# For each number drawn in a draw, count how many of its related numbers
# (e.g., its Bonanza, its Counterpart) were also drawn in the SAME draw.

# Create a temporary df with relationships for each drawn number in each draw
temp_drawn_relationships <- historical_draws_long_df |>
  select(unique_DrawID, Draw_Seq_No, Drawn_Number) |>
  left_join(relationship_df_fe, by = c("Drawn_Number" = "Ref_Number"))

# Initialize feature columns in historical_draws_long_df
for (rel_type in relationship_types) {
  new_feat_col_name <- paste0(tolower(rel_type), "_in_current_count")
  historical_draws_long_df[[new_feat_col_name]] <- 0 # Initialize with 0
}

# Calculate co-occurrence for each relationship type in the current draw
for (rel_type in relationship_types) {
  col_name_val <- sym(rel_type) # Name of the column in relationship_df_fe (e.g., Bonanza)
  new_feat_col_name <- paste0(tolower(rel_type), "_in_current_count")
  
  current_rel_cooccurrence <- temp_drawn_relationships |>
    select(unique_DrawID, Drawn_Number, Related_Value_Col = !!col_name_val) |>
    filter(!is.na(Related_Value_Col)) |> # Only consider if a relationship exists
    left_join(draw_sets |> select(unique_DrawID, S_current), by = "unique_DrawID") |>
    # Explode S_current to check each related value against each number in the draw
    # This can be inefficient if S_current is large. A rowwise operation on the original set might be better
    # For now, using unnest for S_current to allow %in% check
    rowwise() |>
    mutate(is_related_in_current = Related_Value_Col %in% unlist(S_current) & Related_Value_Col != Drawn_Number) |>
    ungroup() |>
    filter(is_related_in_current == TRUE) |> # Keep only co-occurrences
    group_by(unique_DrawID, Drawn_Number) |> # Group back to the original grain
    summarise(!!new_feat_col_name := n(), .groups = 'drop') # Count occurrences
  
  if(nrow(current_rel_cooccurrence) > 0) {
    historical_draws_long_df <- historical_draws_long_df |>
      select(-!!sym(new_feat_col_name)) |> # Remove the initialized column
      left_join(current_rel_cooccurrence, by = c("unique_DrawID", "Drawn_Number")) |>
      mutate(!!sym(new_feat_col_name) := ifelse(is.na(!!sym(new_feat_col_name)), 0, !!sym(new_feat_col_name)))
  }
  # If no co-occurrences, the initialized column of 0s remains.
}

print("Sample of historical_draws_long_df with CURRENT draw relationship features:")
print(str(historical_draws_long_df |> select(unique_DrawID, Drawn_Number, contains("_in_current_count"))))
print(head(historical_draws_long_df |> select(unique_DrawID, Drawn_Number, contains("_in_current_count")) |> filter(bonanza_in_current_count > 0)))


# --- Relationship Features: Related number appeared in PREVIOUS Draw(s) ---
# For each Drawn_Number in draw D (identified by Draw_Seq_No), 
# check if its related numbers appeared in draw D-1 (previous Draw_Seq_No).

# Create lag of S_current (drawn number sets)
draw_sets_with_lags <- draw_sets |>
  arrange(Draw_Seq_No) |>
  mutate(S_previous_1 = lag(S_current, 1)) # Get numbers from the immediately preceding draw sequence

# Join S_previous_1 to the temp_drawn_relationships (which has Drawn_Number and its relationships for each draw)
temp_drawn_relationships_with_prev_draw <- temp_drawn_relationships |>
  left_join(draw_sets_with_lags |> select(Draw_Seq_No, S_previous_1), by = "Draw_Seq_No")

# Initialize feature columns in historical_draws_long_df
for (rel_type in relationship_types) {
  new_feat_col_name_prev1 <- paste0(tolower(rel_type), "_in_prev1_count")
  historical_draws_long_df[[new_feat_col_name_prev1]] <- 0 # Initialize with 0
}

for (rel_type in relationship_types) {
  col_name_val <- sym(rel_type) # Name of the column in relationship_df_fe (e.g., Bonanza)
  new_feat_col_name_prev1 <- paste0(tolower(rel_type), "_in_prev1_count") 
  
  prev_rel_cooccurrence <- temp_drawn_relationships_with_prev_draw |>
    select(unique_DrawID, Drawn_Number, Related_Value_Col = !!col_name_val, S_previous_1) |>
    filter(!is.na(Related_Value_Col) & !sapply(S_previous_1, is.null) & sapply(S_previous_1, length) > 0) |> 
    rowwise() |>
    mutate(is_related_in_prev1 = Related_Value_Col %in% unlist(S_previous_1)) |>
    ungroup() |>
    filter(is_related_in_prev1 == TRUE) |>
    group_by(unique_DrawID, Drawn_Number) |>
    summarise(!!new_feat_col_name_prev1 := n(), .groups = 'drop')
  
  if(nrow(prev_rel_cooccurrence) > 0){
    historical_draws_long_df <- historical_draws_long_df |>
      select(-!!sym(new_feat_col_name_prev1)) |> # Remove initialized column
      left_join(prev_rel_cooccurrence, by = c("unique_DrawID", "Drawn_Number")) |>
      mutate(!!sym(new_feat_col_name_prev1) := ifelse(is.na(!!sym(new_feat_col_name_prev1)), 0, !!sym(new_feat_col_name_prev1)))
  }
}

print("Sample of historical_draws_long_df with PREVIOUS draw relationship features:")
print(str(historical_draws_long_df |> select(unique_DrawID, Drawn_Number, contains("_in_prev1_count"))))
print(head(historical_draws_long_df |> select(unique_DrawID, Drawn_Number, contains("_in_prev1_count")) |> filter(bonanza_in_prev1_count > 0)))

# More advanced patterns like "Shadow of last Bonanza appears next" would require more complex logic,
# potentially looking at relationships *of numbers drawn in the previous draw* and checking their
# secondary relationships against the current draw. This can be built upon these foundational features.

print("Feature engineering from Relationship Table & Combined Features (initial set) complete.")


# --- Data Transformation for Modeling ---
print("--- Data Transformation for Modeling ---")

# We need a dataset where each row is a (draw, potential_number) pair.
# The target is whether that potential_number was drawn in that draw.
# Features must be based on information available *before* that draw.

# 1. Create a base grid of all draws and all potential numbers (1-90)
all_potential_numbers <- 1:90
# Use draw_sequence_df to get all unique, sequenced draws
# Each row in draw_sequence_df represents a draw for which we will make 90 predictions
# Note: draw_sequence_df was created in "Feature Engineering - Historical Draws"
# It contains unique_DrawID, Date, Draw_Seq_No

modeling_base_grid_df <- draw_sequence_df |>
  crossing(Potential_Number = all_potential_numbers) |>
  arrange(Draw_Seq_No, Potential_Number)

print("Modeling Base Grid - Structure and Head:")
print(str(modeling_base_grid_df))
print(head(modeling_base_grid_df))

# 2. Merge Actual Outcomes (Target Variable: was_drawn)
# For each (Draw_Seq_No, Potential_Number) in the grid, was Potential_Number actually drawn?
# We use historical_draws_long_df which contains Drawn_Number for each Draw_Seq_No.
actuals_df <- historical_draws_long_df |>
  select(Draw_Seq_No, Drawn_Number) |> # Drawn_Number is the number that was actually drawn
  mutate(was_drawn = 1) |>
  distinct() 

modeling_df <- modeling_base_grid_df |>
  left_join(actuals_df, by = c("Draw_Seq_No", "Potential_Number" = "Drawn_Number")) |>
  mutate(was_drawn = ifelse(is.na(was_drawn), 0, 1))

print("Modeling DF with Target Variable - Structure and Head:")
print(str(modeling_df))
print(head(modeling_df |> filter(was_drawn == 1), 10)) 
print(head(modeling_df |> filter(Draw_Seq_No == min(Draw_Seq_No)), 5)) 

# 3. Feature Engineering for each (Draw_Seq_No, Potential_Number)
# All features must use data *strictly before* the current Draw_Seq_No.

# Initialize placeholder columns for key features.
# Efficient calculation for the full dataset is complex and typically involves
# more advanced data manipulation techniques (e.g., data.table, optimized window functions, or Rcpp).
# The logic is outlined below for clarity.

# Historical features for the Potential_Number
modeling_df <- modeling_df |>
  mutate(
    feat_freq_past = NA_integer_,          # Frequency of Potential_Number before current draw
    feat_days_last_seen = NA_real_,       # Days since Potential_Number was last seen before current draw
    feat_draws_last_seen = NA_integer_,    # Draws since Potential_Number was last seen before current draw
    feat_lag1_drawn_potential = NA_integer_ # Was Potential_Number drawn in the (Draw_Seq_No - 1) draw?
  )

# Relationship-based features for Potential_Number based on PREVIOUS draw's numbers
for (rel_type in relationship_types) {
  modeling_df[[paste0("feat_rel_", tolower(rel_type), "_in_prev_draw")]] <- NA_integer_
}


# --- Illustrative calculation for a single draw and a single potential number ---
# This demonstrates the logic. Applying this to the whole dataset would require a loop or apply function.
# For example, to calculate features for Draw_Seq_No = 10, Potential_Number = 5:
#   current_draw_seq_example <- 10
#   potential_num_example <- 5
#   current_draw_date_example <- modeling_df |> filter(Draw_Seq_No == current_draw_seq_example) |> pull(Date) |> unique()

#   Past performance of potential_num_example:
#   hist_for_num_before_curr <- historical_draws_long_df |>
#     filter(Draw_Seq_No < current_draw_seq_example & Drawn_Number == potential_num_example)
#   
#   feat_freq_past_ex <- nrow(hist_for_num_before_curr)
#   feat_days_last_seen_ex <- if (feat_freq_past_ex > 0) {
#     as.numeric(difftime(current_draw_date_example, max(hist_for_num_before_curr$Date), units = "days"))
#   } else { NA_real_ }
#   feat_draws_last_seen_ex <- if (feat_freq_past_ex > 0) {
#     (current_draw_seq_example - 1) - max(hist_for_num_before_curr$Draw_Seq_No)
#   } else { NA_integer_ }
#   
#   prev_draw_actual_numbers <- historical_draws_long_df |> 
#                               filter(Draw_Seq_No == (current_draw_seq_example - 1)) |> 
#                               pull(Drawn_Number) |> unique()
#   feat_lag1_drawn_potential_ex <- ifelse(potential_num_example %in% prev_draw_actual_numbers, 1, 0)

#   Relationship features for potential_num_example based on prev_draw_actual_numbers:
#   relationships_of_pot_num <- relationship_df_fe |> filter(Ref_Number == potential_num_example)
#   if(nrow(relationships_of_pot_num) > 0 && length(prev_draw_actual_numbers) > 0){
#     feat_rel_bonanza_in_prev_draw_ex <- ifelse(!is.na(relationships_of_pot_num$Bonanza) && relationships_of_pot_num$Bonanza %in% prev_draw_actual_numbers, 1, 0)
#     # ... and so on for other relationship types ...
#   }
# This illustrates the kind of data lookups needed per row of modeling_df.

# For the purpose of this step, we are defining the *structure* and *intent* of modeling_df.
# The actual population of these features across the ~200k rows would be a separate, optimized batch process.
# Many ML libraries can also handle NAs gracefully (e.g., XGBoost), or imputation strategies can be applied.

# Add draw context features (like day of week, month) from the 'Date' column of the current draw
modeling_df <- modeling_df |>
  mutate(
    draw_month = month(Date),
    draw_day_of_week = wday(Date, label = TRUE, week_start = 1), # Monday is 1
    draw_day_of_month = mday(Date),
    draw_year_quarter = quarter(Date)
  )

# One-hot encode 'Draw' type from historical_draws_df (for the current draw context)
# This is tricky as 'Draw' type is per draw, not per (draw, potential_number) directly.
# We can join it from draw_sequence_df which should have unique_DrawID and thus Draw type if we add it there.
# Or, if we expect 'Draw' type of current draw to be a feature:
draw_info_df <- historical_draws_df |> select(unique_DrawID, Draw_Type_Current = Draw) |> distinct()
modeling_df <- modeling_df |>
  left_join(draw_info_df, by = "unique_DrawID")


print("Modeling DF structure with placeholders for detailed features and added date/draw type features:")
print(str(modeling_df))
print(head(modeling_df,10))


# The crucial part of this step is that `modeling_df` now has the correct shape:
# - Rows: one per (draw_to_predict, potential_number_1_to_90)
# - Target: `was_drawn` (binary)
# - Features: Columns ready to be filled with calculations based on data *prior* to `Draw_Seq_No`.
# The actual filling of these NA features is deferred or would require significant computation time here.

print("Data Transformation for Modeling - structure defined. Key features like past performance and relationship interactions with the previous draw are identified. 
      Date context features added. Full feature calculation would be the next sub-task within a detailed implementation.")

# --- Machine Learning Model Training (XGBoost & Random Forest) ---
print("--- Machine Learning Model Training (XGBoost & Random Forest) ---")

# Install necessary packages if not already present
# if (!requireNamespace("xgboost", quietly = TRUE)) {
#   install.packages("xgboost")
# }
# if (!requireNamespace("randomForest", quietly = TRUE)) {
#   install.packages("randomForest")
# }
# if (!requireNamespace("caret", quietly = TRUE)) {
#   install.packages("caret")
# }
# if (!requireNamespace("pROC", quietly = TRUE)) { # For AUC, often used by caret
#   install.packages("pROC")
# }
# if (!requireNamespace("e1071", quietly = TRUE)) { # Often a caret dependency for some models/preprocessing
#   install.packages("e1071")
# }


library(xgboost)
library(randomForest)
library(caret)
library(pROC) # Loaded for caret's summary functions that might use AUC

# IMPORTANT ASSUMPTION:
# The feature columns (feat_*) in modeling_df are assumed to be populated.
# In the current script, they are NAs. For a real run, these MUST be calculated.
# For this step, we will proceed by:
# 1. Selecting a small subset of features that ARE populated (like date features).
# 2. OR, if we had a way to quickly fill NAs (e.g., with 0 or mean for demonstration).
# Let's try to use the date features and Draw_Type_Current for a minimal example.
# A production model would require the fully engineered `feat_*` columns.

print("WARNING: Training will proceed with a VERY LIMITED feature set or on data with many NAs if not imputed, as detailed historical/relationship features are placeholders.")
print("This step is primarily to demonstrate the model training workflow setup.")

# Data Preparation for Modeling
# Convert target to factor
modeling_df$was_drawn <- as.factor(make.names(modeling_df$was_drawn)) # X0, X1 levels

# Convert categorical features to factors
modeling_df$draw_day_of_week <- as.factor(modeling_df$draw_day_of_week)
modeling_df$Draw_Type_Current <- as.factor(modeling_df$Draw_Type_Current)
# Potential_Number could also be a factor if treated categorically, or numeric.
# For tree-based models, factor representation is fine. For others, numeric might be better or require one-hot.
modeling_df$Potential_Number <- as.factor(modeling_df$Potential_Number)


# Feature selection (Illustrative - using only non-NA placeholders and available features)
# In a real scenario, this would be: names(modeling_df) that start with "feat_" plus date/draw context
# For now, to make it runnable, let's select a small set of *actually populated* features
# AND some of the NA placeholders to show how they *would* be included.
# Caret's preProcess can impute, or XGBoost can handle NAs internally.

feature_names <- c(
  "Potential_Number", 
  "draw_month", 
  "draw_day_of_week", 
  "draw_day_of_month", 
  "draw_year_quarter",
  "Draw_Type_Current",
  # --- These are currently NA, but would be used if populated ---
  "feat_freq_past",            
  "feat_days_last_seen",        
  "feat_draws_last_seen",     
  "feat_lag1_drawn_potential"
  # "feat_rel_bonanza_in_prev_draw" # Example relationship feature
  # ... add all other feat_rel_*_in_prev_draw columns
)
# Add all relationship features for previous draw
for (rel_type in relationship_types) {
  feature_names <- c(feature_names, paste0("feat_rel_", tolower(rel_type), "_in_prev_draw"))
}
feature_names <- unique(feature_names) # Ensure no duplicates

# Ensure selected features exist in modeling_df
feature_names <- feature_names[feature_names %in% names(modeling_df)]

if(length(feature_names) == 0) {
  stop("No valid feature names selected or found in modeling_df. Aborting training.")
}
print(paste("Selected features for training attempt:", paste(feature_names, collapse=", ")))

# Time-based split: Use earlier draws for training, later for testing
# Filter out initial draws if they have too many NAs for recency features (e.g., first draw)
# For this demo, we'll use a simple split. A more robust split considers group integrity for CV.

# Determine a safe starting point for training data (e.g. after some history has accumulated)
# For placeholder features, this might be problematic. Let's assume Draw_Seq_No >= 2 to allow for lag=1 features.
# If feat_ columns were populated, we'd pick a Draw_Seq_No where recency features are mostly non-NA.
safe_start_draw_seq <- 2 
modeling_df_for_split <- modeling_df |> filter(Draw_Seq_No >= safe_start_draw_seq)

if(nrow(modeling_df_for_split) == 0){
  stop(paste("No data available for training after filtering for Draw_Seq_No >=", safe_start_draw_seq))
}

unique_draw_seqs <- sort(unique(modeling_df_for_split$Draw_Seq_No))
if(length(unique_draw_seqs) < 2) { # Need at least 2 unique draw sequences for a split
  stop("Not enough unique draw sequences for a train/test split after initial filtering.")
}
split_point_seq <- unique_draw_seqs[floor(length(unique_draw_seqs) * 0.8)]

train_df <- modeling_df_for_split |> filter(Draw_Seq_No <= split_point_seq)
test_df <- modeling_df_for_split |> filter(Draw_Seq_No > split_point_seq)


if(nrow(train_df) == 0 || nrow(test_df) == 0) {
  stop(paste("Training or testing dataframe is empty after split.",
             "Train rows:", nrow(train_df), "Test rows:", nrow(test_df),
             "Split point seq:", split_point_seq,
             "Min draw seq for training start:", safe_start_draw_seq,
             "Total rows in modeling_df_for_split:", nrow(modeling_df_for_split)))
}

print(paste("Training data rows:", nrow(train_df), "draws up to seq:", max(train_df$Draw_Seq_No, na.rm=T)))
print(paste("Testing data rows:", nrow(test_df), "draws from seq:", min(test_df$Draw_Seq_No, na.rm=T)))


# Select only the chosen features and the target
# We need to handle the fact that feat_ columns are all NA.
# Let's define a minimal feature set that IS populated, for actual model training to run.
minimal_feature_names <- c("Potential_Number", "draw_month", "draw_day_of_week", 
                           "draw_day_of_month", "draw_year_quarter", "Draw_Type_Current")
minimal_feature_names <- minimal_feature_names[minimal_feature_names %in% names(modeling_df)]
print(paste("Using minimal feature set for actual training due to NA placeholders:", paste(minimal_feature_names, collapse = ", ")))


train_data_caret <- train_df[, c(minimal_feature_names, "was_drawn")]
test_data_caret <- test_df[, c(minimal_feature_names, "was_drawn")]


train_control <- trainControl(
  method = "cv", 
  number = 3, 
  classProbs = TRUE, 
  summaryFunction = twoClassSummary,
  allowParallel = FALSE 
)

# --- Random Forest Training ---
print("--- Training Random Forest Model ---")
tune_grid_rf <- expand.grid(.mtry = floor(sqrt(length(minimal_feature_names))))

# Check for NZV on the minimal feature set
nzv_rf <- nearZeroVar(train_data_caret[, minimal_feature_names, drop=FALSE], saveMetrics = TRUE)
feature_names_rf_final <- rownames(nzv_rf[!nzv_rf$nzv,])

if(length(feature_names_rf_final) == 0){
  print("All minimal features removed by NZV for RF. Skipping RF training.")
  rf_model <- NULL
} else {
  print(paste("Final minimal features for RF after NZV:", paste(feature_names_rf_final, collapse = ", ")))
  rf_model <- tryCatch({
    train(
      x = train_data_caret[, feature_names_rf_final, drop = FALSE],
      y = train_data_caret$was_drawn,
      method = "rf",
      trControl = train_control,
      tuneGrid = tune_grid_rf,
      ntree = 20, # Very small number of trees for speed
      metric = "ROC",
      na.action = na.omit # RF in caret needs explicit NA handling or imputation
    )
  }, error = function(e) {
    print(paste("Error training Random Forest:", e$message))
    return(NULL)
  })
}
if (!is.null(rf_model)) { print(rf_model) } else { print("Random Forest model training was skipped or failed.") }


# --- XGBoost Training ---
print("--- Training XGBoost Model ---")
# Check for NZV on the minimal feature set for XGBoost
nzv_xgb <- nearZeroVar(train_data_caret[, minimal_feature_names, drop=FALSE], saveMetrics = TRUE)
feature_names_xgb_final <- rownames(nzv_xgb[!nzv_xgb$nzv,])

if(length(feature_names_xgb_final) == 0){
  print("All minimal features removed by NZV for XGBoost. Skipping XGBoost training.")
  xgb_model <- NULL
} else {
  print(paste("Final minimal features for XGBoost after NZV:", paste(feature_names_xgb_final, collapse = ", ")))
  
  tune_grid_xgb <- expand.grid(
    nrounds = 20, max_depth = 3, eta = 0.3, gamma = 0,
    colsample_bytree = 0.8, min_child_weight = 1, subsample = 0.8
  )
  
  # Convert factor features to numeric for xgboost if not handled by caret's xgbTree
  # Caret's xgbTree usually handles factors by one-hot encoding them, but let's be safe.
  # However, for this specific run, `Potential_Number` might be better as numeric if it has too many levels for OHE.
  # For simplicity, we'll let caret handle it; it often converts factors to dummy vars for xgb.
  
  xgb_model <- tryCatch({
    train(
      x = train_data_caret[, feature_names_xgb_final, drop = FALSE],
      y = train_data_caret$was_drawn,
      method = "xgbTree",
      trControl = train_control,
      tuneGrid = tune_grid_xgb,
      metric = "ROC",
      na.action = na.pass 
    )
  }, error = function(e) {
    print(paste("Error training XGBoost:", e$message))
    return(NULL)
  })
}

if (!is.null(xgb_model)) { print(xgb_model) } else { print("XGBoost model training was skipped or failed.") }

print("ML Model Training step setup complete (actual training used minimal features due to NA placeholders).")
# Placeholder for next steps
print("Next steps: Deep Learning Model Training (Keras)...")


# --- Deep Learning Model Training (Keras - Dense/LSTM) ---
print("--- Deep Learning Model Training (Keras - Dense/LSTM) ---")

# Install Keras and TensorFlow if not already present
if (!requireNamespace("keras", quietly = TRUE)) {
  install.packages("keras")
}
library(keras)

# Check if TensorFlow backend is configured, if not, install Keras (which includes TensorFlow)
# This might require user interaction if it's the first time.
# In a non-interactive environment, this needs to be pre-configured.
# For now, we assume it might try to install if needed.
KERAS_AVAILABLE <- TRUE # Assume available initially, set to FALSE on failure
if (!is_keras_available()) {
  print("Keras backend not found. Attempting to install Keras (TensorFlow)...")
  print("Note: This may require user interaction or take some time.")
  print("If this step fails, subsequent Keras operations will be skipped.")
  tryCatch({
    # Try a CPU-only version for potentially simpler installation in some environments
    install_keras(method = "conda", conda = "auto", tensorflow = "cpu", extra_packages = "tensorflow-hub")
    if (!is_keras_available()) { # Re-check after install attempt
      print("Keras installation attempt finished, but Keras is still not available.")
      KERAS_AVAILABLE <- FALSE
    } else {
      print("Keras successfully installed/found after explicit install call.")
    }
  }, error = function(e) {
    print(paste("Keras/TensorFlow installation failed:", e$message))
    KERAS_AVAILABLE <- FALSE
  })
}

if (KERAS_AVAILABLE) {
  print("Keras backend is available or was successfully installed.")
  # Data Preparation for Keras (using the same train_df, test_df and minimal_feature_names)
  
  # Ensure target 'was_drawn' is numeric 0/1 for Keras
  # train_df and test_df are from the previous ML step
  train_y_keras <- as.numeric(train_df$was_drawn) -1 # Convert X0, X1 to 0, 1
  test_y_keras <- as.numeric(test_df$was_drawn) -1   # Convert X0, X1 to 0, 1
  
  # One-hot encode categorical features using model.matrix
  # Using minimal_feature_names which are known to be populated
  valid_minimal_features_for_keras <- minimal_feature_names[minimal_feature_names %in% names(train_df)]
  
  if(length(valid_minimal_features_for_keras) > 0) {
    formula_keras <- as.formula(paste("~", paste(valid_minimal_features_for_keras, collapse = " + ")))
    
    # Create model matrices
    train_x_keras_matrix_full <- model.matrix(formula_keras, data = train_df)
    test_x_keras_matrix_full <- model.matrix(formula_keras, data = test_df)
    
    # Remove intercept column (usually the first one)
    train_x_keras_matrix <- train_x_keras_matrix_full[, -1, drop = FALSE]
    
    # Ensure test set has the same columns as train set after one-hot encoding
    # Get column names from training matrix (excluding intercept)
    train_cols <- colnames(train_x_keras_matrix)
    
    # For test matrix, ensure it has all columns from train_cols, adding missing ones as 0
    # And select only those columns, in the same order
    test_x_keras_matrix_aligned <- matrix(0, nrow = nrow(test_x_keras_matrix_full), ncol = length(train_cols),
                                          dimnames = list(NULL, train_cols))
    
    # Identify common columns between the full test matrix (after model.matrix) and train_cols
    # model.matrix on test data might produce different columns if factor levels differ or are absent
    original_test_cols_no_intercept <- colnames(test_x_keras_matrix_full)[-1]
    common_cols_in_test <- intersect(train_cols, original_test_cols_no_intercept)
    
    if(length(common_cols_in_test) > 0) {
      test_x_keras_matrix_aligned[, common_cols_in_test] <- test_x_keras_matrix_full[, common_cols_in_test, drop = FALSE]
    }
    test_x_keras_matrix <- test_x_keras_matrix_aligned
    
    
    if(ncol(train_x_keras_matrix) == 0 || is.null(ncol(train_x_keras_matrix))) { # Check if any columns remain
      print("No feature columns remaining after one-hot encoding for Keras. Skipping Keras training.")
      KERAS_AVAILABLE <- FALSE # Update flag to prevent further Keras ops
    } else {
      print(paste("Keras input shape (train):", nrow(train_x_keras_matrix), "x", ncol(train_x_keras_matrix)))
      print(paste("Keras input shape (test):", nrow(test_x_keras_matrix), "x", ncol(test_x_keras_matrix)))
      
      # Define a simple Dense Neural Network
      keras_model_dense <- keras_model_sequential(name = "dense_lottery_model") |>
        layer_dense(units = 64, activation = "relu", input_shape = ncol(train_x_keras_matrix)) |>
        layer_dropout(rate = 0.3) |>
        layer_dense(units = 32, activation = "relu") |>
        layer_dropout(rate = 0.2) |>
        layer_dense(units = 1, activation = "sigmoid")
      
      summary(keras_model_dense)
      
      # Compile the model
      keras_model_dense |> compile(
        optimizer = optimizer_adam(),
        loss = "binary_crossentropy",
        metrics = c("accuracy", tf$keras$metrics$AUC(name = 'auc')) # Using tf$keras$metrics for newer TF versions
      )
      
      # Train the model
      print("--- Training Keras Dense Model ---")
      history_dense <- tryCatch({
        keras_model_dense |> fit(
          train_x_keras_matrix,
          train_y_keras,
          epochs = 3, # Very few epochs for quick demo
          batch_size = 512, 
          validation_split = 0.2, 
          verbose = 1
        )
      }, error = function(e) {
        print(paste("Error training Keras Dense model:", e$message))
        # Check if it's a common "Python package not found" type error
        if(grepl("ModuleNotFoundError", e$message, ignore.case = TRUE) || grepl("conda environment", e$message, ignore.case=TRUE)){
          print("This error often indicates issues with the Python environment Keras is trying to use.")
        }
        return(NULL)
      })
      
      if (!is.null(history_dense)) {
        print(history_dense)
        # Evaluate on test set
        print("--- Evaluating Keras Dense Model on Test Set ---")
        scores_dense <- tryCatch({
          keras_model_dense |> evaluate(test_x_keras_matrix, test_y_keras, verbose = 0)
        }, error = function(e_eval){
          print(paste("Error evaluating Keras model:", e_eval$message))
          return(NULL)
        })
        if(!is.null(scores_dense)){
          print(paste("Keras Dense Test Loss:", scores_dense[[1]]))
          print(paste("Keras Dense Test Accuracy:", scores_dense[[2]]))
          print(paste("Keras Dense Test AUC:", scores_dense[[3]])) # Assuming AUC is the 3rd metric
        }
      } else {
        print("Keras Dense model training failed or was skipped.")
      }
    }
  } else {
    print("No valid minimal features found for Keras training after filtering. Skipping.")
    KERAS_AVAILABLE <- FALSE
  }
} else {
  print("Skipping Deep Learning step as Keras is not available or installation failed.")
}

# LSTM Considerations (Conceptual for now):
# - Input data would need to be reshaped into sequences (samples, timesteps, features_per_timestep).
# - This might involve taking the last N draws for each number, or sequences of numbers within a draw.
# - Categorical features like 'Potential_Number' or relationship types would typically be embedded using an Embedding layer.
# - Example LSTM structure:
#   keras_model_lstm <- keras_model_sequential() |>
#     layer_embedding(input_dim = 91, output_dim = 10, input_length = max_sequence_length) |> # For numbers 1-90
#     layer_lstm(units = 32, return_sequences = TRUE) |>
#     layer_lstm(units = 16) |>
#     layer_dense(units = 1, activation = "sigmoid")
#   # This is highly dependent on how sequences are constructed.

print("Deep Learning Model Training (Keras) step setup complete.")
# Placeholder for next steps
print("Next steps: Pattern Analysis Layer...")


# --- Integration and Output ---
print("--- Integration and Output ---")

# This step simulates how predictions would be generated for a new draw,
# how model outputs might be combined, and how pattern analysis could be integrated.

# Assume:
# - `rf_model`, `xgb_model`, `keras_model_dense` are trained (even if on minimal features for this demo).
# - `relationship_df` is available.
# - `historical_draws_long_df` and `draw_sets_with_lags` are available for context.
# - `train_data_caret` (used for getting factor levels for prediction alignment) is available.
# - `minimal_feature_names`, `feature_names_rf_final`, `feature_names_xgb_final` are available.
# - `KERAS_AVAILABLE`, `keras_model_dense`, `train_x_keras_matrix`, `valid_minimal_features_for_keras` are available for Keras part.

# --- 1. Simulate Feature Generation for a "Next Draw" ---
# To predict for a draw after the last one in our dataset.
# Let's identify the last known Draw_Seq_No and its numbers.
if (exists("draw_sequence_df") && nrow(draw_sequence_df) > 0 && 
    exists("historical_draws_long_df") && nrow(historical_draws_long_df) > 0 &&
    exists("draw_sets_with_lags") && nrow(draw_sets_with_lags) > 0) {
  
  last_draw_seq_in_data <- max(draw_sequence_df$Draw_Seq_No)
  last_draw_date_in_data <- draw_sequence_df |> filter(Draw_Seq_No == last_draw_seq_in_data) |> pull(Date) |> first() # Use first() if multiple unique_DrawID have same max seq
  
  print(paste("Last draw in dataset: Seq_No =", last_draw_seq_in_data, "on Date", last_draw_date_in_data))
  
  s_last_actual_draw_list <- draw_sets_with_lags |> 
    filter(Draw_Seq_No == last_draw_seq_in_data) |> 
    # S_current for the last draw_seq_no is what we need
    # If multiple unique_DrawID share the max Draw_Seq_No (e.g. different lotteries on same day), pick one
    slice(1) |> 
    pull(S_current) 
  
  s_last_actual_draw <- NULL
  if (length(s_last_actual_draw_list) > 0 && is.list(s_last_actual_draw_list)) {
    s_last_actual_draw <- unlist(s_last_actual_draw_list[[1]])
  }
  
  
  if(is.null(s_last_actual_draw) || length(s_last_actual_draw) == 0) {
    s_last_actual_draw <- historical_draws_long_df |> 
      filter(Draw_Seq_No == last_draw_seq_in_data) |>
      pull(Drawn_Number) |> unique()
    print("Used fallback to get s_last_actual_draw.")
  }
  
  if(!is.null(s_last_actual_draw) && length(s_last_actual_draw) > 0){
    print(paste("Numbers in last actual draw (Seq_No", last_draw_seq_in_data, "):", paste(s_last_actual_draw, collapse=", ")))
  } else {
    print(paste("Warning: Could not determine numbers for the last actual draw (Seq_No", last_draw_seq_in_data,"). Pattern analysis might be affected."))
    s_last_actual_draw <- c() # Ensure it's an empty vector not NULL for pattern functions
  }
  
  next_draw_date_assumed <- last_draw_date_in_data + days(1) 
  last_draw_type_df <- historical_draws_df |> arrange(desc(Date), desc(unique_DrawID)) |> slice(1)
  last_draw_type <- if(nrow(last_draw_type_df) > 0) last_draw_type_df$Draw else "Unknown"
  
  
  predict_next_draw_df <- tibble(
    Potential_Number = 1:90,
    draw_month = month(next_draw_date_assumed),
    draw_day_of_week = as.factor(wday(next_draw_date_assumed, label = TRUE, week_start = 1)),
    draw_day_of_month = mday(next_draw_date_assumed),
    draw_year_quarter = quarter(next_draw_date_assumed),
    Draw_Type_Current = as.factor(last_draw_type) 
  )
  predict_next_draw_df$Potential_Number <- as.factor(predict_next_draw_df$Potential_Number)
  
  # --- 2. Simulate Model Predictions ---
  # Ensure factor levels are consistent with training data
  if(exists("train_data_caret")){
    for(col_name in intersect(names(predict_next_draw_df), names(train_data_caret))){
      if(is.factor(train_data_caret[[col_name]])){
        predict_next_draw_df[[col_name]] <- factor(predict_next_draw_df[[col_name]], levels = levels(train_data_caret[[col_name]]))
      }
    }
  } else {
    print("Warning: train_data_caret not found; factor level alignment for prediction might be incorrect.")
  }
  
  
  if (exists("rf_model") && !is.null(rf_model) && exists("feature_names_rf_final")) {
    predict_next_draw_df_rf_subset <- predict_next_draw_df[, feature_names_rf_final, drop = FALSE]
    # Imputation for RF prediction (if preproc_rf was defined and used knnImpute)
    # For simplicity, if NAs arise from factor mismatch, predict might produce NAs or error.
    # A robust solution involves saving preprocessor or careful factor handling.
    # Here, we assume features are mostly complete or model handles them.
    pred_rf_probs <- tryCatch({
      predict(rf_model, newdata = predict_next_draw_df_rf_subset, type = "prob")$X1
    }, error = function(e) { 
      print(paste("RF Prediction Error:", e$message)); rep(runif(1,0,0.1), nrow(predict_next_draw_df_rf_subset)) 
    })
    predict_next_draw_df$prob_rf <- pred_rf_probs
  } else {
    print("RF model or feature names not found. Using random RF probabilities.")
    predict_next_draw_df$prob_rf <- runif(90, 0, 0.1)
  }
  
  if (exists("xgb_model") && !is.null(xgb_model) && exists("feature_names_xgb_final")) {
    predict_next_draw_df_xgb_subset <- predict_next_draw_df[, feature_names_xgb_final, drop = FALSE]
    pred_xgb_probs <- tryCatch({
      predict(xgb_model, newdata = predict_next_draw_df_xgb_subset, type = "prob", na.action = na.pass)$X1
    }, error = function(e) {
      print(paste("XGB Prediction Error:", e$message)); rep(runif(1,0,0.1), nrow(predict_next_draw_df_xgb_subset))
    })
    predict_next_draw_df$prob_xgb <- pred_xgb_probs
  } else {
    print("XGB model or feature names not found. Using random XGB probabilities.")
    predict_next_draw_df$prob_xgb <- runif(90, 0, 0.1)
  }
  
  if (exists("KERAS_AVAILABLE") && KERAS_AVAILABLE && exists("keras_model_dense") && !is.null(keras_model_dense) && 
      exists("train_x_keras_matrix") && exists("valid_minimal_features_for_keras")) {
    
    formula_keras_pred <- as.formula(paste("~", paste(valid_minimal_features_for_keras, collapse = " + ")))
    predict_x_keras_matrix_full <- model.matrix(formula_keras_pred, data = predict_next_draw_df)
    predict_x_keras_matrix_no_intercept <- predict_x_keras_matrix_full[, -1, drop = FALSE]
    
    train_cols_keras <- colnames(train_x_keras_matrix) 
    predict_x_keras_matrix_aligned <- matrix(0, nrow = nrow(predict_x_keras_matrix_no_intercept), ncol = length(train_cols_keras),
                                             dimnames = list(NULL, train_cols_keras))
    common_cols_pred_keras <- intersect(train_cols_keras, colnames(predict_x_keras_matrix_no_intercept))
    
    if(length(common_cols_pred_keras) > 0){
      predict_x_keras_matrix_aligned[, common_cols_pred_keras] <- predict_x_keras_matrix_no_intercept[, common_cols_pred_keras, drop = FALSE]
    }
    
    pred_keras_probs <- tryCatch({
      as.vector(predict(keras_model_dense, predict_x_keras_matrix_aligned))
    }, error = function(e) {
      print(paste("Keras Prediction Error:", e$message)); runif(nrow(predict_next_draw_df),0,0.1)
    })
    predict_next_draw_df$prob_keras <- pred_keras_probs
  } else {
    print("Keras model/data not available. Using random Keras probabilities.")
    predict_next_draw_df$prob_keras <- runif(90, 0, 0.1)
  }
  
  # --- 3. Model Ensemble (Simple Average) ---
  predict_next_draw_df$prob_ensemble_avg <- rowMeans(
    select(predict_next_draw_df, starts_with("prob_")), 
    na.rm = TRUE
  )
  
  # --- 4. Incorporate Pattern Analysis ---
  predict_next_draw_df$pattern_score_boost <- 0.0
  if(length(s_last_actual_draw) > 0){ # Only apply if we have numbers from last draw
    for (i in 1:nrow(predict_next_draw_df)) {
      current_potential_num_val <- as.numeric(levels(predict_next_draw_df$Potential_Number[i]))[predict_next_draw_df$Potential_Number[i]]
      boost <- 0
      if (check_pattern_shadow_of_prev_bonanza(current_potential_num_val, s_last_actual_draw, relationship_df)) {
        boost <- boost + 0.05 
      }
      if (check_pattern_counterpart_of_prev_turning(current_potential_num_val, s_last_actual_draw, relationship_df)) {
        boost <- boost + 0.03 
      }
      predict_next_draw_df$pattern_score_boost[i] <- boost
    }
  }
  
  predict_next_draw_df$final_score <- predict_next_draw_df$prob_ensemble_avg + predict_next_draw_df$pattern_score_boost
  predict_next_draw_df$final_score <- pmin(pmax(predict_next_draw_df$final_score, 0), 1)
  
  # --- 5. Final Output Generation ---
  ranked_predictions <- predict_next_draw_df |>
    mutate(Potential_Number_Num = as.numeric(levels(Potential_Number))[Potential_Number]) |> # Get numeric value back
    arrange(desc(final_score)) |>
    select(Potential_Number = Potential_Number_Num, final_score, prob_rf, prob_xgb, prob_keras, pattern_score_boost)
  
  print("--- Top 20 Ranked Predictions for Next Draw ---")
  print(head(ranked_predictions, 20))
  
} else {
  print("Not enough historical data or draw_sets_with_lags not found to generate predictions for a 'next' draw.")
}

print("Integration and Output step conceptually complete.")
# Placeholder for next steps
print("Next steps: Submit the change (or further refinement).")


# --- Pattern Analysis Layer ---
print("--- Pattern Analysis Layer ---")

# This layer is intended to add heuristic adjustments or identify specific patterns
# that might not be easily captured by standard ML features alone.

# We need:
# 1. `relationship_df` (already loaded and preprocessed)
# 2. `historical_draws_long_df` (to get sets of numbers for previous draws)
# 3. `draw_sets_with_lags` (created in relationship feature engineering, has S_current and S_previous_1)
#    (Ensure draw_sets_with_lags is available or re-create if necessary, it was created in the combined features step)
if (!exists("draw_sets_with_lags")) {
  print("Re-creating draw_sets_with_lags for Pattern Analysis Layer...")
  draw_sets_temp <- historical_draws_long_df |>
    group_by(unique_DrawID, Draw_Seq_No) |>
    summarise(S_current = list(unique(Drawn_Number)), .groups = 'drop') |>
    arrange(Draw_Seq_No)
  
  draw_sets_with_lags <- draw_sets_temp |>
    arrange(Draw_Seq_No) |>
    mutate(S_previous_1 = lag(S_current, 1))
}


# Example Pattern 1: "Shadow of a Bonanza from Previous Draw"
# If number X was in S_previous_1, and Y is X's Bonanza, is Potential_Number the Shadow of Y?
check_pattern_shadow_of_prev_bonanza <- function(potential_num, s_previous_1_numbers, rel_df) {
  if (is.null(s_previous_1_numbers) || length(s_previous_1_numbers) == 0 || NROW(s_previous_1_numbers) == 0 || all(is.na(s_previous_1_numbers))) {
    return(FALSE)
  }
  
  for (num_in_prev_draw in s_previous_1_numbers) {
    if(is.na(num_in_prev_draw)) next # Skip if NA somehow got into the list
    
    bonanza_of_num_in_prev_df <- rel_df |> filter(Number == num_in_prev_draw)
    if(nrow(bonanza_of_num_in_prev_df) == 0 || is.na(bonanza_of_num_in_prev_df$Bonanza)) next
    bonanza_val <- bonanza_of_num_in_prev_df$Bonanza
    
    shadow_of_bonanza_df <- rel_df |> filter(Number == bonanza_val)
    if(nrow(shadow_of_bonanza_df) == 0 || is.na(shadow_of_bonanza_df$shadow)) next
    shadow_val <- shadow_of_bonanza_df$shadow
    
    if (shadow_val == potential_num) {
      return(TRUE) # Pattern detected
    }
  }
  return(FALSE) # Pattern not detected
}

# Example Pattern 2: "Counterpart of a Turning from Previous Draw"
# If number X was in S_previous_1, and Y is X's Turning, is Potential_Number the Counterpart of Y?
check_pattern_counterpart_of_prev_turning <- function(potential_num, s_previous_1_numbers, rel_df) {
  if (is.null(s_previous_1_numbers) || length(s_previous_1_numbers) == 0 || NROW(s_previous_1_numbers) == 0 || all(is.na(s_previous_1_numbers))) {
    return(FALSE)
  }
  
  for (num_in_prev_draw in s_previous_1_numbers) {
    if(is.na(num_in_prev_draw)) next
    
    turning_of_num_in_prev_df <- rel_df |> filter(Number == num_in_prev_draw)
    if(nrow(turning_of_num_in_prev_df) == 0 || is.na(turning_of_num_in_prev_df$Turning)) next
    turning_val <- turning_of_num_in_prev_df$Turning
    
    counterpart_of_turning_df <- rel_df |> filter(Number == turning_val)
    if(nrow(counterpart_of_turning_df) == 0 || is.na(counterpart_of_turning_df$Counterparts)) next
    counterpart_val <- counterpart_of_turning_df$Counterparts
    
    if (counterpart_val == potential_num) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Apply these pattern checks to the modeling_df
# This will add new columns indicating if a pattern was met for that (Draw_Seq_No, Potential_Number)
# This is a conceptual application; for performance, vectorization or more advanced apply methods would be better.

# We need to iterate row by row for modeling_df, get the S_previous_1 for its Draw_Seq_No,
# then apply the checks. This is slow.
# A more optimized way:
# 1. Get all unique (Draw_Seq_No, S_previous_1) pairs.
# 2. Join this to modeling_df.
# 3. Then apply row-wise or grouped operations.

modeling_df_with_patterns <- modeling_df |>
  left_join(draw_sets_with_lags |> select(Draw_Seq_No, S_previous_1), by = "Draw_Seq_No")

# Initialize pattern columns
modeling_df_with_patterns$pattern_shadow_bon_prev <- FALSE
modeling_df_with_patterns$pattern_count_turn_prev <- FALSE

# Iterate and apply - this is for illustration and will be slow on the full dataset
# A small subset for demonstration:
# sample_draw_seqs_for_pattern <- unique(modeling_df_with_patterns$Draw_Seq_No)[1:3] # first 3 draws
# if(is.na(sample_draw_seqs_for_pattern[1])) sample_draw_seqs_for_pattern <- unique(modeling_df_with_patterns$Draw_Seq_No)[2:4]


# Instead of a full loop, let's demonstrate on one draw (e.g., Draw_Seq_No = 2)
target_pattern_draw_seq <- 2
if (target_pattern_draw_seq <= max(modeling_df_with_patterns$Draw_Seq_No, na.rm=TRUE) && 
    target_pattern_draw_seq > min(modeling_df_with_patterns$Draw_Seq_No, na.rm=TRUE)) {
  
  s_prev_numbers_for_target_draw <- modeling_df_with_patterns |>
    filter(Draw_Seq_No == target_pattern_draw_seq) |>
    pull(S_previous_1) |>
    first() |> # Get the list of numbers
    unlist()
  
  if (!is.null(s_prev_numbers_for_target_draw) && length(s_prev_numbers_for_target_draw) > 0) {
    print(paste("Numbers in draw", target_pattern_draw_seq - 1, "(S_previous for draw", target_pattern_draw_seq, "):", paste(s_prev_numbers_for_target_draw, collapse=", ")))
    
    rows_for_target_draw <- which(modeling_df_with_patterns$Draw_Seq_No == target_pattern_draw_seq)
    
    for(i in rows_for_target_draw) {
      potential_num_current <- modeling_df_with_patterns$Potential_Number[i]
      modeling_df_with_patterns$pattern_shadow_bon_prev[i] <- 
        check_pattern_shadow_of_prev_bonanza(potential_num_current, s_prev_numbers_for_target_draw, relationship_df)
      modeling_df_with_patterns$pattern_count_turn_prev[i] <- 
        check_pattern_counterpart_of_prev_turning(potential_num_current, s_prev_numbers_for_target_draw, relationship_df)
    }
    
    print(paste("Pattern analysis for Draw_Seq_No =", target_pattern_draw_seq, "(predicting for this draw based on previous):"))
    print(head(modeling_df_with_patterns |> 
                 filter(Draw_Seq_No == target_pattern_draw_seq) |>
                 select(Potential_Number, was_drawn, pattern_shadow_bon_prev, pattern_count_turn_prev) |>
                 filter(pattern_shadow_bon_prev | pattern_count_turn_prev), 10))
  } else {
    print(paste("Could not retrieve numbers for draw", target_pattern_draw_seq - 1, "to apply pattern analysis for draw", target_pattern_draw_seq,"."))
  }
} else {
  print("Not enough draws or target_pattern_draw_seq is out of bounds to demonstrate pattern analysis.")
}

# The full modeling_df would now have these two new boolean columns (mostly FALSE, TRUE where patterns hit)
# These could be used as features in models or to adjust scores post-prediction.

# For integration, one might define a scoring adjustment:
# final_score = model_prob + (pattern_shadow_bon_prev * 0.1) + (pattern_count_turn_prev * 0.05) - etc.

print("Pattern Analysis Layer conceptual setup and limited application complete.")
# Placeholder for next steps
print("Next steps: Integration and Output...")
