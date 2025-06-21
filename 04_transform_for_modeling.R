# --- 04_transform_for_modeling.R ---
# Purpose: Transform data into the final structure required for model training.
# This typically means one row per (draw, potential_number) pair, with a binary target.
# Features for each row are calculated based on information available *before* that draw.
# Depends on: historical_draws_long_df, draw_sequence_df, relationship_df, all_potential_numbers (global or defined here)
# Outputs: modeling_df (the main data frame for training models)

print("--- Stage 4: Data Transformation for Modeling ---")

# Ensure necessary libraries are loaded
if (!requireNamespace("dplyr", quietly = TRUE)) { install.packages("dplyr") }
if (!requireNamespace("tidyr", quietly = TRUE)) { install.packages("tidyr") } # For crossing
if (!requireNamespace("lubridate", quietly = TRUE)) { install.packages("lubridate") }
library(dplyr)
library(tidyr)
library(lubridate)

# Define all potential numbers if not already defined globally
if (!exists("all_potential_numbers")) {
  all_potential_numbers <- 1:90
}
# Ensure relationship_types is available (defined in 03_feature_eng_relationships.R, or define here if run standalone)
if (!exists("relationship_types")) {
    relationship_types <- c("Bonanza", "Counterparts", "Malta", "String_Key",
                            "shadow", "Partner", "Equivalent", "Turning")
}


# 1. Create a base grid: all draws x all potential numbers
# `draw_sequence_df` (unique_DrawID, Date, Draw_Seq_No) should exist from 02_feature_eng_historical.R
if (!exists("draw_sequence_df")) {
  stop("draw_sequence_df not found. Run 02_feature_eng_historical.R first.")
}
modeling_base_grid_df <- draw_sequence_df %>%
  crossing(Potential_Number = all_potential_numbers) %>%
  arrange(Draw_Seq_No, Potential_Number)

print("Modeling Base Grid - Structure and Head:")
print(str(modeling_base_grid_df))
print(head(modeling_base_grid_df, 3))

# 2. Merge Actual Outcomes (Target Variable: was_drawn)
# `historical_draws_long_df` (unique_DrawID, Date, Draw, Drawn_Number, Draw_Seq_No, ...) should exist
if (!exists("historical_draws_long_df")) {
  stop("historical_draws_long_df not found. Run 02_feature_eng_historical.R and 03_feature_eng_relationships.R first.")
}
actuals_df <- historical_draws_long_df %>%
  select(Draw_Seq_No, Drawn_Number) %>%
  mutate(was_drawn = 1) %>%
  distinct()

modeling_df <- modeling_base_grid_df %>%
  left_join(actuals_df, by = c("Draw_Seq_No", "Potential_Number" = "Drawn_Number")) %>%
  mutate(was_drawn = ifelse(is.na(was_drawn), 0, 1))

print("Modeling DF with Target Variable - Structure and Sample:")
print(str(modeling_df))
print(head(modeling_df %>% filter(was_drawn == 1), 3))
print(head(modeling_df %>% filter(Draw_Seq_No == min(Draw_Seq_No, na.rm=TRUE)), 3))

# 3. Feature Engineering for each (Draw_Seq_No, Potential_Number) pair
# IMPORTANT: All features must use data *strictly before* the current Draw_Seq_No.
# The calculation of these features is complex and computationally intensive.
# The code below initializes these columns as NA.
# A full implementation would require dedicated functions and optimized operations
# to populate these based on historical_draws_long_df and relationship_df.

print("Initializing placeholder columns for detailed historical and relationship-based lag features...")
print("WARNING: These features are NOT being calculated in this script due to complexity and performance.")
print("They are essential for a meaningful model and would need to be implemented robustly.")

# Historical features for the Potential_Number
modeling_df <- modeling_df %>%
  mutate(
    feat_freq_past = NA_integer_,          # Cumulative frequency of Potential_Number before current draw
    feat_days_last_seen = NA_real_,       # Days since Potential_Number was last seen (before current draw's date)
    feat_draws_last_seen = NA_integer_,    # Draws since Potential_Number was last seen (before current_draw_seq)
    feat_lag1_drawn_potential = NA_integer_ # Was Potential_Number drawn in (Draw_Seq_No - 1)?
    # Add more lag features (lag2, lag3...)
    # Add rolling frequencies (e.g., count in last 10, 20, 50 draws before current)
  )

# Relationship-based features for Potential_Number based on PREVIOUS draw's numbers
# Example: feat_rel_bonanza_in_prev_draw = 1 if Bonanza of Potential_Number was in (Draw_Seq_No - 1), else 0
for (rel_type in relationship_types) {
  modeling_df[[paste0("feat_rel_", tolower(rel_type), "_in_prev_draw_count")]] <- NA_integer_
}

# Add draw context features (from the current draw's date and type)
modeling_df <- modeling_df %>%
  mutate(
    draw_month = month(Date),
    draw_day_of_week = wday(Date, label = TRUE, week_start = 1),
    draw_day_of_month = mday(Date),
    draw_year_quarter = quarter(Date),
    draw_year = year(Date)
  )

# Add Draw_Type_Current for the draw we are predicting for
# `historical_draws_df` contains `unique_DrawID` and `Draw` (lottery type)
if (exists("historical_draws_df")) {
    draw_type_info <- historical_draws_df %>%
        select(unique_DrawID, Draw_Type_Current = Draw) %>%
        distinct()
    modeling_df <- modeling_df %>%
        left_join(draw_type_info, by = "unique_DrawID")
} else {
    print("Warning: historical_draws_df not found to add Draw_Type_Current to modeling_df.")
    modeling_df$Draw_Type_Current <- NA_character_
}


print("Modeling DF structure with placeholders for detailed features and added date/draw type context:")
print(str(modeling_df))
print(head(modeling_df, 5))

# Note on Populating Features:
# The `feat_*` columns are crucial. Their calculation involves:
# For each row (target_draw_seq, potential_num) in `modeling_df`:
#   1. Filter `historical_draws_long_df` for `Draw_Seq_No < target_draw_seq`.
#   2. From this past data, calculate frequency, recency, lags for `potential_num`.
#   3. Identify numbers drawn in `target_draw_seq - 1`.
#   4. For `potential_num`, find its relatives from `relationship_df`.
#   5. Check if these relatives were in numbers from `target_draw_seq - 1`.
# This process needs to be carefully implemented to avoid data leakage from the future.

print("--- Stage 4: Data Transformation for Modeling Complete (structure defined, features need full calculation) ---")
