# --- 03_feature_eng_relationships.R ---
# Purpose: Feature engineering based on the relationship table and combined with historical context.
# Creates features like co-occurrence of related numbers in current or previous draws.
# Depends on: historical_draws_long_df, relationship_df, draw_sequence_df (implicitly via historical_draws_long_df)
# Outputs: historical_draws_long_df (augmented with new relationship features), draw_sets_with_lags

print("--- Stage 3: Feature Engineering - Relationship Table & Combined Features ---")

# Ensure necessary libraries are loaded
if (!requireNamespace("dplyr", quietly = TRUE)) { install.packages("dplyr") }
if (!requireNamespace("tidyr", quietly = TRUE)) { install.packages("tidyr") } # For pivot_longer if used
library(dplyr)
library(tidyr)

# Relationship columns to use for feature generation
# (Make sure this list matches the columns in your relationship_df)
relationship_types <- c("Bonanza", "Counterparts", "Malta", "String_Key",
                        "shadow", "Partner", "Equivalent", "Turning")

# Rename relationship_df's 'Number' column to avoid conflict and clarify its role as reference
# This change is local to this script's context if relationship_df is not passed back.
# If it's modified globally, ensure this is intended. For now, assume it's a local copy or modification.
# To be safe, let's work with a copy for this renaming if relationship_df is global.
relationship_df_fe <- relationship_df %>% rename(Ref_Number = Number)


# Create a helper data frame: for each draw, list the set of numbers drawn (S_current)
# This is used for checking co-occurrences.
# It uses Draw_Seq_No which should be present in historical_draws_long_df from the previous step.
draw_sets <- historical_draws_long_df %>%
  group_by(unique_DrawID, Draw_Seq_No) %>%
  summarise(S_current = list(unique(Drawn_Number)), .groups = 'drop') %>%
  arrange(Draw_Seq_No)

# --- A. Relationship Features: Co-occurrence of related numbers in the CURRENT Draw ---
# For each number drawn in a draw, count how many of its relatives were also drawn in the SAME draw.

# Create a temporary df: each drawn number in each draw, along with all its relationships
temp_drawn_relationships <- historical_draws_long_df %>%
  select(unique_DrawID, Draw_Seq_No, Drawn_Number) %>%
  left_join(relationship_df_fe, by = c("Drawn_Number" = "Ref_Number"))

# Initialize new feature columns in historical_draws_long_df
for (rel_type in relationship_types) {
  new_feat_col_name <- paste0(tolower(rel_type), "_in_current_count")
  historical_draws_long_df[[new_feat_col_name]] <- 0
}

# Calculate co-occurrence for each relationship type
for (rel_type in relationship_types) {
  col_name_val <- sym(rel_type)
  new_feat_col_name <- paste0(tolower(rel_type), "_in_current_count")

  # Summarize co-occurrences
  current_rel_cooccurrence <- temp_drawn_relationships %>%
    filter(!is.na(!!col_name_val)) %>% # Ensure the relationship value itself is not NA
    left_join(draw_sets %>% select(unique_DrawID, S_current), by = "unique_DrawID") %>%
    rowwise() %>%
    mutate(is_related_in_current = (!!col_name_val) %in% unlist(S_current) & (!!col_name_val) != Drawn_Number) %>%
    ungroup() %>%
    filter(is_related_in_current == TRUE) %>%
    group_by(unique_DrawID, Drawn_Number) %>%
    summarise(!!new_feat_col_name := n(), .groups = 'drop')

  # Join results back if any co-occurrences were found
  if(nrow(current_rel_cooccurrence) > 0) {
    # To correctly update, we need to remove the initialized column first if using left_join for updates
    historical_draws_long_df <- historical_draws_long_df %>% select(-!!sym(new_feat_col_name))
    historical_draws_long_df <- historical_draws_long_df %>%
      left_join(current_rel_cooccurrence, by = c("unique_DrawID", "Drawn_Number")) %>%
      mutate(!!sym(new_feat_col_name) := ifelse(is.na(!!sym(new_feat_col_name)), 0, !!sym(new_feat_col_name)))
  }
  # If no co-occurrences for this rel_type, the column remains 0s as initialized.
}

print("Sample of historical_draws_long_df with CURRENT draw relationship features (first few rows with any hit):")
print(str(historical_draws_long_df %>% select(unique_DrawID, Drawn_Number, Draw_Seq_No, ends_with("_in_current_count"))))
if (any(rowSums(select(historical_draws_long_df, ends_with("_in_current_count"))) > 0)) {
   print(head(historical_draws_long_df %>% filter(if_any(ends_with("_in_current_count"), ~ . > 0)) %>%
             select(unique_DrawID, Drawn_Number, Draw_Seq_No, ends_with("_in_current_count")), 10))
} else {
   print("No co-occurrences found for 'in_current_count' features in the sample/dataset.")
}


# --- B. Relationship Features: Related number appeared in PREVIOUS Draw(s) ---
# For each Drawn_Number in draw D, check if its related numbers appeared in draw D-1.

# Create lag of S_current (drawn number sets) for S_previous_1
draw_sets_with_lags <- draw_sets %>%
  arrange(Draw_Seq_No) %>%
  mutate(S_previous_1 = lag(S_current, 1))

# Join S_previous_1 to the temp_drawn_relationships
temp_drawn_relationships_with_prev_draw <- temp_drawn_relationships %>%
  left_join(draw_sets_with_lags %>% select(Draw_Seq_No, S_previous_1), by = "Draw_Seq_No")

# Initialize new feature columns
for (rel_type in relationship_types) {
  new_feat_col_name_prev1 <- paste0(tolower(rel_type), "_in_prev1_count")
  historical_draws_long_df[[new_feat_col_name_prev1]] <- 0
}

# Calculate co-occurrence for each relationship type with previous draw
for (rel_type in relationship_types) {
  col_name_val <- sym(rel_type)
  new_feat_col_name_prev1 <- paste0(tolower(rel_type), "_in_prev1_count")

  prev_rel_cooccurrence <- temp_drawn_relationships_with_prev_draw %>%
    filter(!is.na(!!col_name_val) & !sapply(S_previous_1, is.null) & sapply(S_previous_1, length) > 0) %>%
    rowwise() %>%
    mutate(is_related_in_prev1 = (!!col_name_val) %in% unlist(S_previous_1)) %>% # Note: Here we don't exclude if Related_Value_Col == Drawn_Number, as it's about previous draw
    ungroup() %>%
    filter(is_related_in_prev1 == TRUE) %>%
    group_by(unique_DrawID, Drawn_Number) %>%
    summarise(!!new_feat_col_name_prev1 := n(), .groups = 'drop')

  if(nrow(prev_rel_cooccurrence) > 0){
    historical_draws_long_df <- historical_draws_long_df %>% select(-!!sym(new_feat_col_name_prev1))
    historical_draws_long_df <- historical_draws_long_df %>%
      left_join(prev_rel_cooccurrence, by = c("unique_DrawID", "Drawn_Number")) %>%
      mutate(!!sym(new_feat_col_name_prev1) := ifelse(is.na(!!sym(new_feat_col_name_prev1)), 0, !!sym(new_feat_col_name_prev1)))
  }
}

print("Sample of historical_draws_long_df with PREVIOUS draw relationship features (first few rows with any hit):")
print(str(historical_draws_long_df %>% select(unique_DrawID, Drawn_Number, Draw_Seq_No, ends_with("_in_prev1_count"))))
if (any(rowSums(select(historical_draws_long_df, ends_with("_in_prev1_count"))) > 0)) {
    print(head(historical_draws_long_df %>% filter(if_any(ends_with("_in_prev1_count"), ~ . > 0)) %>%
             select(unique_DrawID, Drawn_Number, Draw_Seq_No, ends_with("_in_prev1_count")), 10))
} else {
   print("No co-occurrences found for '_in_prev1_count' features in the sample/dataset.")
}

# Note: More complex, multi-step relationship patterns (e.g., "shadow of a bonanza from two draws ago")
# can be built by extending these principles, typically involving more lag terms and joins.

print("--- Stage 3: Feature Engineering - Relationship Table & Combined Features Complete ---")
