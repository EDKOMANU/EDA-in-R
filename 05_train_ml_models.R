# --- 05_train_ml_models.R ---
# Purpose: Train Machine Learning models (XGBoost, Random Forest).
# Depends on: modeling_df from 04_transform_for_modeling.R
# Outputs: Trained rf_model, xgb_model objects (or NULL if errors).

print("--- Stage 5: Machine Learning Model Training (XGBoost & Random Forest) ---")

# Install necessary packages if not already present
# Note: The main orchestrator script should handle one-time global package installations.
# These checks are for potential standalone script execution.
if (!requireNamespace("xgboost", quietly = TRUE)) { install.packages("xgboost") }
if (!requireNamespace("randomForest", quietly = TRUE)) { install.packages("randomForest") }
if (!requireNamespace("caret", quietly = TRUE)) { install.packages("caret") }
if (!requireNamespace("pROC", quietly = TRUE)) { install.packages("pROC") } # For AUC
if (!requireNamespace("e1071", quietly = TRUE)) { install.packages("e1071") } # caret dependency

library(xgboost)
library(randomForest)
library(caret)
library(pROC)

# Initialize model objects to NULL
rf_model <- NULL
xgb_model <- NULL

# IMPORTANT ASSUMPTION:
# The `feat_*` feature columns in `modeling_df` (e.g., `feat_freq_past`) are assumed
# to be populated by an earlier, more detailed version of `04_transform_for_modeling.R`.
# The current `04_transform_for_modeling.R` only creates these as NA placeholders.
# Training will proceed with a minimal set of *actually populated* features if `feat_*` are all NA.
print("WARNING: Model training quality depends heavily on populated 'feat_*' columns.")
print("If 'feat_*' columns are all NA, training will use a minimal feature set.")

if (!exists("modeling_df") || nrow(modeling_df) == 0) {
  stop("modeling_df not found or is empty. Run previous steps first.")
}

# Data Preparation for Modeling
modeling_df_ml <- modeling_df # Create a copy for this script

# Convert target to factor (X0, X1 levels for caret)
modeling_df_ml$was_drawn <- as.factor(make.names(modeling_df_ml$was_drawn))

# Convert relevant categorical features to factors
# Date parts are often better as numeric for some models, but factor for others.
# Tree-based models can handle factors well.
factor_cols <- c("draw_day_of_week", "Draw_Type_Current", "Potential_Number",
                 "draw_month", "draw_year_quarter") # Treating month/quarter as categorical too
for(col in factor_cols){
    if(col %in% names(modeling_df_ml)) {
        modeling_df_ml[[col]] <- as.factor(modeling_df_ml[[col]])
    }
}
# Ensure draw_day_of_month and draw_year are numeric
if("draw_day_of_month" %in% names(modeling_df_ml)) modeling_df_ml$draw_day_of_month <- as.numeric(modeling_df_ml$draw_day_of_month)
if("draw_year" %in% names(modeling_df_ml)) modeling_df_ml$draw_year <- as.numeric(modeling_df_ml$draw_year)


# Define feature sets
all_feat_cols <- grep("^feat_", names(modeling_df_ml), value = TRUE)
contextual_cols <- c("Potential_Number", "draw_month", "draw_day_of_week",
                     "draw_day_of_month", "draw_year_quarter", "draw_year", "Draw_Type_Current")
contextual_cols <- contextual_cols[contextual_cols %in% names(modeling_df_ml)]


# Check if detailed features are populated (i.e., not all NA)
detailed_features_populated <- FALSE
if (length(all_feat_cols) > 0) {
  # Check if at least one 'feat_' column has non-NA values in a sample
  # (checking all rows of all columns can be slow)
  sample_feat_data <- modeling_df_ml %>% sample_n(min(1000, nrow(modeling_df_ml))) %>% select(all_of(all_feat_cols))
  if (any(!sapply(sample_feat_data, function(x) all(is.na(x))))) {
    detailed_features_populated <- TRUE
  }
}

if (detailed_features_populated) {
  print("Detailed 'feat_*' features seem to be populated. Using them along with contextual features.")
  feature_names_for_training <- unique(c(contextual_cols, all_feat_cols))
} else {
  print("Detailed 'feat_*' features are placeholders (all NA). Using only contextual features for training.")
  feature_names_for_training <- contextual_cols
}
feature_names_for_training <- feature_names_for_training[feature_names_for_training %in% names(modeling_df_ml)]

if(length(feature_names_for_training) == 0) {
  stop("No valid feature names available for training. Aborting.")
}
print(paste("Features selected for training:", paste(feature_names_for_training, collapse=", ")))


# Time-based split
# Using a safe_start_draw_seq to ensure some history for any implicit lags/recency if features were full
safe_start_draw_seq <- if (detailed_features_populated) min(modeling_df_ml$Draw_Seq_No, na.rm=TRUE) + 5 else 2 # Small offset if using detailed feats
modeling_df_for_split <- modeling_df_ml %>% filter(Draw_Seq_No >= safe_start_draw_seq)

if(nrow(modeling_df_for_split) == 0){
    stop(paste("No data available for training after filtering for Draw_Seq_No >=", safe_start_draw_seq))
}
unique_draw_seqs <- sort(unique(modeling_df_for_split$Draw_Seq_No))
if(length(unique_draw_seqs) < 2) {
    stop("Not enough unique draw sequences for a train/test split after filtering.")
}
split_point_seq <- unique_draw_seqs[floor(length(unique_draw_seqs) * 0.8)]

train_df <- modeling_df_for_split %>% filter(Draw_Seq_No <= split_point_seq)
test_df <- modeling_df_for_split %>% filter(Draw_Seq_No > split_point_seq)

if(nrow(train_df) == 0 || nrow(test_df) == 0) {
    stop("Training or testing dataframe is empty after split.")
}
print(paste("Training data rows:", nrow(train_df)))
print(paste("Testing data rows:", nrow(test_df)))

train_data_caret <- train_df[, c(feature_names_for_training, "was_drawn")]
# test_data_caret <- test_df[, c(feature_names_for_training, "was_drawn")] # For later evaluation

# caret Training Control
train_control <- trainControl(
  method = "cv",
  number = 3, # 3-fold CV; increase for better estimate, but slower
  classProbs = TRUE,
  summaryFunction = twoClassSummary, # Uses ROC, Sensitivity, Specificity
  allowParallel = FALSE # Set to TRUE if parallel backend is registered
)

# --- Random Forest Training ---
print("--- Training Random Forest Model ---")
# RF needs NAs to be handled. Impute if detailed features are used and might have NAs.
# For contextual features only, NAs are less likely unless from joins.
train_data_rf <- train_data_caret
preproc_rf_for_na <- NULL
if (detailed_features_populated && any(sapply(train_data_rf[, feature_names_for_training], function(x) any(is.na(x))))) {
    print("Imputing NAs for Random Forest using median/mode.")
    # Note: knnImpute can be very slow. medianImpute/bagImpute are alternatives.
    preproc_rf_for_na <- preProcess(train_data_rf[, feature_names_for_training, drop = FALSE], method = c("medianImpute")) # "modeImpute" for factors if needed
    train_data_rf <- predict(preproc_rf_for_na, train_data_rf)
}

# Remove Near Zero Variance predictors
nzv_rf <- nearZeroVar(train_data_rf[, feature_names_for_training, drop=FALSE], saveMetrics = TRUE)
feature_names_rf_final <- rownames(nzv_rf[!nzv_rf$nzv,])

if(length(feature_names_rf_final) == 0){
    print("All features removed by NZV for RF. Skipping RF training.")
} else {
    print(paste("Final features for RF after NZV:", paste(feature_names_rf_final, collapse = ", ")))
    tune_grid_rf <- expand.grid(.mtry = floor(sqrt(length(feature_names_rf_final))))
    rf_model <- tryCatch({
        train(
            x = train_data_rf[, feature_names_rf_final, drop = FALSE],
            y = train_data_rf$was_drawn,
            method = "rf",
            trControl = train_control,
            tuneGrid = tune_grid_rf,
            ntree = 25, # Small ntree for speed
            metric = "ROC",
            na.action = na.omit # Should be handled by imputation if needed
        )
    }, error = function(e) {
        print(paste("Error training Random Forest:", e$message)); NULL
    })
}
if (!is.null(rf_model)) { print(rf_model) } else { print("Random Forest model training was skipped or failed.") }


# --- XGBoost Training ---
print("--- Training XGBoost Model ---")
# XGBoost can handle NAs internally if `na.action = na.pass` is used or if NAs are passed directly.
# Caret's default might be to omit.
nzv_xgb <- nearZeroVar(train_data_caret[, feature_names_for_training, drop=FALSE], saveMetrics = TRUE) # Use original train_data_caret for NZV
feature_names_xgb_final <- rownames(nzv_xgb[!nzv_xgb$nzv,])

if(length(feature_names_xgb_final) == 0){
    print("All features removed by NZV for XGBoost. Skipping XGBoost training.")
} else {
    print(paste("Final features for XGBoost after NZV:", paste(feature_names_xgb_final, collapse = ", ")))
    tune_grid_xgb <- expand.grid(
      nrounds = 25, max_depth = 3, eta = 0.3, gamma = 0,
      colsample_bytree = 0.8, min_child_weight = 1, subsample = 0.8
    )
    xgb_model <- tryCatch({
      train(
        x = train_data_caret[, feature_names_xgb_final, drop = FALSE], # Use original train_data_caret
        y = train_data_caret$was_drawn,
        method = "xgbTree",
        trControl = train_control,
        tuneGrid = tune_grid_xgb,
        metric = "ROC",
        na.action = na.pass # Explicitly tell caret to pass NAs to xgbTree if supported by wrapper
      )
    }, error = function(e) {
      print(paste("Error training XGBoost:", e$message)); NULL
    })
}
if (!is.null(xgb_model)) { print(xgb_model) } else { print("XGBoost model training was skipped or failed.") }

print("--- Stage 5: Machine Learning Model Training Complete (or attempted) ---")
