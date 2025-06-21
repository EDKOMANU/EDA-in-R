# --- 02_feature_eng_historical.R ---
# Purpose: Feature engineering based on historical draw data.
# Creates features like frequency, recency, and draw intervals.
# Depends on: cleaned historical_draws_df from 01_preprocess_clean.R
# Outputs: historical_draws_long_df, number_total_frequency_df, draw_sequence_df

print("--- Stage 2: Feature Engineering - Historical Draws ---")

# Ensure necessary libraries are loaded
if (!requireNamespace("dplyr", quietly = TRUE)) { install.packages("dplyr") }
if (!requireNamespace("tidyr", quietly = TRUE)) { install.packages("tidyr") }
if (!requireNamespace("lubridate", quietly = TRUE)) { install.packages("lubridate") }
library(dplyr)
library(tidyr) # For unnest
library(lubridate)

# 1. Transform Data to Long Format for easier feature calculation per number
# Unnest the Numbers_List to have one row per drawn number per draw
historical_draws_long_df <- historical_draws_df %>%
  select(unique_DrawID, Date, Draw, Numbers_List) %>% # Select relevant columns
  unnest(Numbers_List) %>%
  rename(Drawn_Number = Numbers_List) %>%
  # Ensure chronological order before any window functions or lag calculations
  arrange(Date, unique_DrawID, Drawn_Number)

print("Historical Draws Long Format - Structure & Head:")
print(str(historical_draws_long_df))
print(head(historical_draws_long_df))

# 2. Calculate Overall Frequency of each number (as a global statistic)
number_total_frequency_df <- historical_draws_long_df %>%
  group_by(Drawn_Number) %>%
  summarise(Total_Appearances = n(), .groups = 'drop') %>%
  arrange(Drawn_Number)

print("Overall Frequency of each number (global statistic):")
print(head(number_total_frequency_df))

# 3. Create a sequential draw number for calculating draw-based recency/intervals
# This uses the unique_DrawID which was made unique and sorted by Date in preprocessing.
draw_sequence_df <- historical_draws_df %>%
  select(unique_DrawID, Date) %>% # Date is kept for joining if needed, unique_DrawID is key
  distinct() %>%
  arrange(Date, unique_DrawID) %>%
  mutate(Draw_Seq_No = row_number())

print("Draw Sequence DF - Head:")
print(head(draw_sequence_df))

# Join Draw_Seq_No to the long dataframe
historical_draws_long_df <- historical_draws_long_df %>%
  left_join(draw_sequence_df %>% select(unique_DrawID, Date, Draw_Seq_No), by = c("unique_DrawID", "Date")) %>%
  # Critical: Re-sort by Drawn_Number, then by time (Draw_Seq_No) for correct lag calculation
  arrange(Drawn_Number, Draw_Seq_No)

# 4. Calculate Recency (days_since_last_drawn) and Draw Interval (draws_since_last_drawn)
# These features are calculated for each time a number *appears*.
historical_draws_long_df <- historical_draws_long_df %>%
  group_by(Drawn_Number) %>%
  mutate(
    Last_Draw_Date = lag(Date, 1),
    Last_Draw_Seq_No = lag(Draw_Seq_No, 1)
  ) %>%
  ungroup() %>%
  mutate(
    Days_Since_Last_Drawn = if_else(!is.na(Last_Draw_Date),
                                    as.numeric(difftime(Date, Last_Draw_Date, units = "days")),
                                    NA_real_),
    Draws_Since_Last_Drawn = if_else(!is.na(Last_Draw_Seq_No),
                                     Draw_Seq_No - Last_Draw_Seq_No,
                                     NA_integer_) # This is the draw interval for this specific number
  ) %>%
  select(-Last_Draw_Date, -Last_Draw_Seq_No) # Clean up temporary lag columns

print("Historical Draws Long Format with Recency/Interval - Structure & Sample:")
print(str(historical_draws_long_df))
print("Sample for Drawn_Number == 1 (to check recency/interval features):")
if (1 %in% historical_draws_long_df$Drawn_Number) {
    print(head(historical_draws_long_df %>% filter(Drawn_Number == 1), 10))
} else {
    print("Number 1 not found in the dataset to show sample.")
}

# Note: Rolling frequencies (e.g., "count in last X draws") and true lag features
# (e.g., "was number N drawn in the *absolute* previous calendar draw?") for all 90 numbers
# are typically constructed in the 'Data Transformation for Modeling' phase, as they depend
# on the structure of one-row-per-potential-number-per-draw.
# The current features are building blocks for that stage.

print("--- Stage 2: Feature Engineering - Historical Draws Complete ---")
