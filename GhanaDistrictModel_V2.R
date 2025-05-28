# --- Ghana Multi-District Population Model - Version 2 ---
#
# Purpose:
# This script demonstrates a multi-district population projection model.
# Version 2 focuses on illustrating the integration of statistically estimated
# demographic rate coefficients derived from SIMULATED historical data.
# It shows a conceptual workflow from data generation, to coefficient estimation,
# to using those estimates in a forward-looking population simulation.
#
# Key Sections:
# I.   Generation of Simulated Historical Data: Creates a sample 'historical' dataset.
# II.  Estimation of Model Coefficients: Uses lm() on simulated data to estimate coefficients.
# III. Simulation Model Parameters: Populates simulation parameters using these estimates.
# IV.  Core Simulation Functions: Defines how demographic events and migration occur.
# V.   Simulation Loop: Runs the projection over time.
# VI.  Results Output: Displays and plots simulation results.
# VII. Documentation and Caveats: Further notes and explanations.
#
# Note: This version is for demonstration of the workflow. In a real application,
# Section I would be replaced by actual historical data collection and cleaning,
# and Section II would involve more rigorous statistical modeling.

# --- Section I: Generation of Simulated Historical Data ---
# Purpose: To create a synthetic dataset that mimics plausible historical trends
#          for our districts. This data will then be used in Section II to
#          'estimate' the coefficients for the demographic rate functions,
#          demonstrating the data-to-model pipeline.
# Note: In a real research project, this section would be replaced by the
#       loading and pre-processing of actual observed historical data.

# Define "True" Underlying Parameters for Data Generation
# These are 'secrets' used only to generate the data; Section II will try to estimate them.
message("--- Section I: Initializing True Parameters for Historical Data Generation ---")
hist_true_base_fert_D1 <- 0.045  # True base fertility for District D1
hist_true_base_fert_D2 <- 0.050  # True base fertility for District D2
hist_true_base_fert_D3 <- 0.040  # True base fertility for District D3

hist_true_base_mort_D1 <- 0.015  # True base mortality for District D1
hist_true_base_mort_D2 <- 0.012  # True base mortality for District D2
hist_true_base_mort_D3 <- 0.018  # True base mortality for District D3

hist_true_coeff_fert_health <- -0.00015 # True effect of health score on fertility (better health -> lower fertility)
hist_true_coeff_fert_econ <- -0.00025  # True effect of econ score on fertility (better econ -> lower fertility)
hist_true_coeff_mort_health <- -0.00020 # True effect of health score on mortality (better health -> lower mortality)
hist_true_coeff_mort_econ <- -0.00015  # True effect of econ score on mortality (better econ -> lower mortality)

historical_years <- 25          # Number of years to simulate for historical data
noise_level_fert <- 0.002       # Standard deviation of random noise for fertility rates
noise_level_mort <- 0.001       # Standard deviation of random noise for mortality rates

# Initialize Data Storage for Historical Data
simulated_historical_df <- data.frame()

# Loop to Generate Data for Each District and Year
district_ids <- c("D1", "D2", "D3") # Define district_ids globally for use in later sections
base_fert_rates_true <- c(hist_true_base_fert_D1, hist_true_base_fert_D2, hist_true_base_fert_D3)
base_mort_rates_true <- c(hist_true_base_mort_D1, hist_true_base_mort_D2, hist_true_base_mort_D3)

message("Generating simulated historical data...")
for (d_idx in 1:length(district_ids)) {
  current_district_id <- district_ids[d_idx]
  current_base_fert <- base_fert_rates_true[d_idx]
  current_base_mort <- base_mort_rates_true[d_idx]
  
  for (year_hist in 1:historical_years) {
    health_score_hist <- 40 + (year_hist * 1.0) + rnorm(1, mean=0, sd=5) 
    health_score_hist <- max(10, min(90, health_score_hist))             
    econ_score_hist <- 35 + (year_hist * 1.2) + rnorm(1, mean=0, sd=7)   
    econ_score_hist <- max(10, min(90, econ_score_hist))                 
    
    fert_rate_hist <- current_base_fert + 
                      (health_score_hist * hist_true_coeff_fert_health) + 
                      (econ_score_hist * hist_true_coeff_fert_econ) + 
                      rnorm(1, mean=0, sd=noise_level_fert)
    fert_rate_hist <- max(0.005, fert_rate_hist) 
    
    mort_rate_hist <- current_base_mort + 
                      (health_score_hist * hist_true_coeff_mort_health) + 
                      (econ_score_hist * hist_true_coeff_mort_econ) + 
                      rnorm(1, mean=0, sd=noise_level_mort)
    mort_rate_hist <- max(0.001, mort_rate_hist) 
    
    year_data <- data.frame(
      Year = year_hist, DistrictID = current_district_id, 
      ActualHealthScore = health_score_hist, ActualEconScore = econ_score_hist, 
      GeneratedFertilityRate = fert_rate_hist, GeneratedMortalityRate = mort_rate_hist
    )
    simulated_historical_df <- rbind(simulated_historical_df, year_data)
  }
}
message("--- Section I: Simulated Historical Data Generation Complete ---")
message(paste("Generated", nrow(simulated_historical_df), "rows of historical data over", historical_years, "years for", length(district_ids), "districts."))
message("Sample of generated historical data (first 6 rows):")
print(head(simulated_historical_df))
message("Summary of generated historical data:")
print(summary(simulated_historical_df))
message("--- End of Section I ---")

# --- Section II: Estimation of Model Coefficients from Simulated Historical Data ---
# Purpose: To use statistical models (lm()) to estimate relationships using 'simulated_historical_df'.
message("--- Section II.A: Estimating Fertility Model Coefficients ---")
fertility_estimation_model <- lm(GeneratedFertilityRate ~ ActualHealthScore + ActualEconScore + DistrictID, 
                                 data = simulated_historical_df)
message("Summary of Fertility Estimation Model:")
print(summary(fertility_estimation_model))
est_coeffs_fertility <- coef(fertility_estimation_model)
est_coeff_fert_health_sim <- est_coeffs_fertility["ActualHealthScore"]
est_coeff_fert_econ_sim <- est_coeffs_fertility["ActualEconScore"]
message(paste("Estimated coefficient for Health Score on Fertility (est_coeff_fert_health_sim):", round(est_coeff_fert_health_sim, 6)))
message(paste("Estimated coefficient for Econ Score on Fertility (est_coeff_fert_econ_sim):", round(est_coeff_fert_econ_sim, 6)))

message("--- Section II.B: Estimating Mortality Model Coefficients ---")
mortality_estimation_model <- lm(GeneratedMortalityRate ~ ActualHealthScore + ActualEconScore + DistrictID, 
                                 data = simulated_historical_df)
message("Summary of Mortality Estimation Model:")
print(summary(mortality_estimation_model))
est_coeffs_mortality <- coef(mortality_estimation_model)
est_coeff_mort_health_sim <- est_coeffs_mortality["ActualHealthScore"]
est_coeff_mort_econ_sim <- est_coeffs_mortality["ActualEconScore"]
message(paste("Estimated coefficient for Health Score on Mortality (est_coeff_mort_health_sim):", round(est_coeff_mort_health_sim, 6)))
message(paste("Estimated coefficient for Econ Score on Mortality (est_coeff_mort_econ_sim):", round(est_coeff_mort_econ_sim, 6)))

message("--- Section II.C: Migration Model Coefficients (Placeholder) ---")
message("Migration parameter estimation from historical data is not implemented in V2. Assumed parameters will be used for the simulation model.")
message("--- End of Section II ---")

# --- Section III: Simulation Model Parameters (Derived from Estimation & Assumptions) ---
# Purpose: To define the parameters that will drive the forward-looking simulation.
#          Key demographic coefficients and base rates are derived from the
#          statistical estimations in Section II. Other parameters (e.g., for migration,
#          policies, simulation duration) are set here.

message("--- Section III.A: Populating Simulation Coefficients from Estimated Values ---")
# Demographic Rate Coefficients (EFFECTS of indicators - derived from Section II)
# The lm output gives direct effect: e.g. fertility = intercept + B1*Health + B2*Econ...
# Our dynamic rate functions use: base_rate - coeff_effect*Score.
# So, if lm B1 is negative (good health -> lower fert), our coeff_effect should be positive.
# Therefore, coeff_effect_sim = -1 * (estimated_lm_coefficient).
coeff_fert_health_effect_sim <- -1 * est_coeff_fert_health_sim # From Section II.A
coeff_fert_econ_effect_sim <- -1 * est_coeff_fert_econ_sim     # From Section II.A
coeff_mort_health_effect_sim <- -1 * est_coeff_mort_health_sim # From Section II.B
coeff_mort_econ_effect_sim <- -1 * est_coeff_mort_econ_sim     # From Section II.B

message(paste("Sim coeff_fert_health_effect_sim:", round(coeff_fert_health_effect_sim, 6)))
message(paste("Sim coeff_fert_econ_effect_sim:", round(coeff_fert_econ_effect_sim, 6)))
message(paste("Sim coeff_mort_health_effect_sim:", round(coeff_mort_health_effect_sim, 6)))
message(paste("Sim coeff_mort_econ_effect_sim:", round(coeff_mort_econ_effect_sim, 6)))

message("--- Section III.B: Defining Initial State for Forward Simulation ---")
last_historical_year_data <- subset(simulated_historical_df, Year == historical_years & DistrictID %in% district_ids)

sim_initial_population_D1 <- 60000 
sim_initial_population_D2 <- 80000
sim_initial_population_D3 <- 35000
initial_populations_sim <- c(D1=sim_initial_population_D1, D2=sim_initial_population_D2, D3=sim_initial_population_D3)

initial_health_scores_sim <- numeric(length(district_ids))
initial_econ_scores_sim <- numeric(length(district_ids))
for(i in 1:length(district_ids)){
  d_id <- district_ids[i]
  last_hist_row_for_dist <- last_historical_year_data[as.character(last_historical_year_data$DistrictID) == d_id, ]
  if(nrow(last_hist_row_for_dist) == 1){
    initial_health_scores_sim[i] <- last_hist_row_for_dist$ActualHealthScore
    initial_econ_scores_sim[i] <- last_hist_row_for_dist$ActualEconScore
  } else {
    warning(paste("Could not find unique last historical year data for DistrictID:", d_id, "- using NA for initial scores."))
    initial_health_scores_sim[i] <- NA
    initial_econ_scores_sim[i] <- NA
  }
}

base_fert_rates_sim <- numeric(length(district_ids))
base_mort_rates_sim <- numeric(length(district_ids))
for(i in 1:length(district_ids)){
  d_id <- district_ids[i]
  newdata_pred <- data.frame(ActualHealthScore=0, ActualEconScore=0, 
                             DistrictID=factor(d_id, levels=levels(simulated_historical_df$DistrictID)))
  base_fert_rates_sim[i] <- predict(fertility_estimation_model, newdata=newdata_pred)
  base_mort_rates_sim[i] <- predict(mortality_estimation_model, newdata=newdata_pred)
}
message(paste("Estimated Base Fertility Rates for Simulation (D1, D2, D3):", paste(round(base_fert_rates_sim, 4), collapse=", ")))
message(paste("Estimated Base Mortality Rates for Simulation (D1, D2, D3):", paste(round(base_mort_rates_sim, 4), collapse=", ")))

districts_df <- data.frame(
  DistrictID = factor(district_ids, levels=district_ids),
  InitialPopulation = initial_populations_sim[match(district_ids, names(initial_populations_sim))], # Ensure correct order
  BaseFertilityRate = base_fert_rates_sim,
  BaseMortalityRate = base_mort_rates_sim,
  HealthcareAccessScore = initial_health_scores_sim,
  EconomicOpportunityScore = initial_econ_scores_sim
)
message("Simulation `districts_df` (initial state for projection period):")
print(districts_df)
# SCALABILITY NOTE: For many districts, manual definition of initial populations and extraction
# of base rates would be replaced by programmatic loops and data merging. Base rates could be
# stored in a lookup table or calculated and merged based on DistrictID.

message("--- Section III.C: Other Simulation Parameters ---")
migration_health_weight <- 0.4 
migration_econ_weight <- 0.6   
base_migration_propensity <- 0.005 
projection_simulation_years <- 30  

policy1_sim <- list(year = 10, district_id = "D1", indicator_name = "HealthcareAccessScore", new_value = 85)
policy2_sim <- list(year = 15, district_id = "D2", indicator_name = "EconomicOpportunityScore", new_value = 90)
policies_list_sim <- list(policy1_sim, policy2_sim) 
# SCALABILITY NOTE: Policy definitions could be from external files or scenario builders.

results_log_df <- data.frame(
  Year = integer(), DistrictID = factor(levels=district_ids), Population = numeric(),
  DynamicFertilityRate = numeric(), DynamicMortalityRate = numeric(),
  Births = numeric(), Deaths = numeric(), NetMigration = numeric(),
  HealthcareAccessScore = numeric(), EconomicOpportunityScore = numeric()
)
message("--- End of Section III ---")

# --- IV. Core Simulation Functions ---
# Note: These functions operate on parameters defined in Section III.

# Section 4.1: Dynamic Demographic Rate Functions
calculate_dynamic_fertility_rate <- function(base_fertility, health_score, econ_score, 
                                             c_fert_h_effect_sim, c_fert_e_effect_sim) {
  fertility_adjustment <- (health_score * c_fert_h_effect_sim) + (econ_score * c_fert_e_effect_sim)
  dynamic_rate <- base_fertility - fertility_adjustment
  dynamic_rate <- max(0, dynamic_rate)
  return(dynamic_rate)
}

calculate_dynamic_mortality_rate <- function(base_mortality, health_score, econ_score, 
                                              c_mort_h_effect_sim, c_mort_e_effect_sim) {
  mortality_adjustment <- (health_score * c_mort_h_effect_sim) + (econ_score * c_mort_e_effect_sim)
  dynamic_rate <- base_mortality - mortality_adjustment
  dynamic_rate <- max(0, dynamic_rate)
  return(dynamic_rate)
}

# Section 4.2: Migration Functions
calculate_district_attractiveness <- function(health_score, econ_score, health_weight, econ_weight) {
  attractiveness <- (health_score * health_weight) + (econ_score * econ_weight)
  return(attractiveness)
}

calculate_interdistrict_migration <- function(current_districts_data, health_w, econ_w, base_propensity) {
  num_districts <- nrow(current_districts_data)
  mig_calc_df <- current_districts_data 
  mig_calc_df$Attractiveness <- numeric(num_districts)
  for (k in 1:num_districts) {
    mig_calc_df$Attractiveness[k] <- calculate_district_attractiveness(
      health_score = mig_calc_df$HealthcareAccessScore[k],
      econ_score = mig_calc_df$EconomicOpportunityScore[k],
      health_weight = health_w, econ_weight = econ_w
    )
  }
  net_migration_final <- numeric(num_districts)
  names(net_migration_final) <- mig_calc_df$DistrictID
  for (i in 1:num_districts) {
    for (j in 1:num_districts) {
      if (i == j) next
      attractiveness_difference_j_vs_i <- mig_calc_df$Attractiveness[j] - mig_calc_df$Attractiveness[i]
      if (attractiveness_difference_j_vs_i > 0) {
        flow_i_to_j <- attractiveness_difference_j_vs_i * mig_calc_df$Population[i] * base_propensity
        flow_i_to_j <- min(flow_i_to_j, mig_calc_df$Population[i] * 0.50)
        net_migration_final[i] <- net_migration_final[i] - flow_i_to_j 
        net_migration_final[j] <- net_migration_final[j] + flow_i_to_j 
      }
    }
  }
  return(net_migration_final)
}

# Section 4.3: Policy Function
apply_district_policy <- function(current_districts_status_df, list_of_policies, current_sim_year) {
  updated_districts_df <- current_districts_status_df 
  for (policy in list_of_policies) {
    if (policy$year == current_sim_year) {
      row_index <- which(as.character(updated_districts_df$DistrictID) == as.character(policy$district_id))
      if (length(row_index) == 1) { 
        indicator_to_change <- policy$indicator_name
        new_indicator_value <- policy$new_value
        if (indicator_to_change %in% names(updated_districts_df)) {
          old_value <- updated_districts_df[row_index, indicator_to_change]
          updated_districts_df[row_index, indicator_to_change] <- new_indicator_value
          message(paste0("Policy Applied in Year ", current_sim_year, 
                         ": District ", policy$district_id, 
                         "'s '", indicator_to_change, 
                         "' changed from ", round(old_value, 2),
                         " to ", round(new_indicator_value, 2), "."))
        } else {
          warning(paste0("Policy Warning: Indicator '", indicator_to_change, "' not found for District ", policy$district_id))
        }
      } else {  warning(paste0("Policy Warning: District '", policy$district_id, "' not found or not unique.")) }
    }
  }
  return(updated_districts_df)
}
# --- End of IV. Core Simulation Functions ---

# --- V. Simulation Loop ---
message("--- Starting Forward Projection Simulation ---")

# Section 5.1: Initialize Current State for Forward Simulation
current_districts_status_df <- districts_df 
if ("InitialPopulation" %in% names(current_districts_status_df)) {
    names(current_districts_status_df)[names(current_districts_status_df) == "InitialPopulation"] <- "Population"
}
num_all_districts <- nrow(current_districts_status_df)
results_log_df <- results_log_df[0,] 

# Section 5.2: Main Loop through Simulation Years (Forward Projection)
for (year_proj in 1:projection_simulation_years) { 
  message(paste("--- Simulating Projection Year:", year_proj, "of", projection_simulation_years, "---"))
  current_districts_status_df <- apply_district_policy(
    current_districts_status_df = current_districts_status_df,
    list_of_policies = policies_list_sim, 
    current_sim_year = year_proj
  )
  current_dynamic_fertility_rates <- numeric(num_all_districts)
  current_dynamic_mortality_rates <- numeric(num_all_districts)
  births_this_year <- numeric(num_all_districts)
  deaths_this_year <- numeric(num_all_districts)
  for (i in 1:num_all_districts) {
    district_data_row <- current_districts_status_df[i, ]
    dynamic_fert_rate <- calculate_dynamic_fertility_rate(
      base_fertility = district_data_row$BaseFertilityRate, 
      health_score = district_data_row$HealthcareAccessScore,
      econ_score = district_data_row$EconomicOpportunityScore,
      c_fert_h_effect_sim = coeff_fert_health_effect_sim, 
      c_fert_e_effect_sim = coeff_fert_econ_effect_sim    
    )
    current_dynamic_fertility_rates[i] <- dynamic_fert_rate
    dynamic_mort_rate <- calculate_dynamic_mortality_rate(
      base_mortality = district_data_row$BaseMortalityRate, 
      health_score = district_data_row$HealthcareAccessScore,
      econ_score = district_data_row$EconomicOpportunityScore,
      c_mort_h_effect_sim = coeff_mort_health_effect_sim, 
      c_mort_e_effect_sim = coeff_mort_econ_effect_sim    
    )
    current_dynamic_mortality_rates[i] <- dynamic_mort_rate
    num_births <- district_data_row$Population * dynamic_fert_rate
    births_this_year[i] <- num_births
    num_deaths <- district_data_row$Population * dynamic_mort_rate
    deaths_this_year[i] <- num_deaths
    current_districts_status_df$Population[i] <- current_districts_status_df$Population[i] + num_births - num_deaths
    current_districts_status_df$Population[i] <- max(0, current_districts_status_df$Population[i]) 
  }
  net_migration_vector <- calculate_interdistrict_migration(
    current_districts_data = current_districts_status_df, 
    health_w = migration_health_weight,    
    econ_w = migration_econ_weight,        
    base_propensity = base_migration_propensity 
  )
  for (i in 1:num_all_districts) {
    current_districts_status_df$Population[i] <- current_districts_status_df$Population[i] + net_migration_vector[i]
    current_districts_status_df$Population[i] <- max(0, current_districts_status_df$Population[i]) 
  }
  for (j in 1:num_all_districts) {
    log_row <- data.frame(
      Year = year_proj, DistrictID = current_districts_status_df$DistrictID[j],
      Population = current_districts_status_df$Population[j], 
      DynamicFertilityRate = current_dynamic_fertility_rates[j],
      DynamicMortalityRate = current_dynamic_mortality_rates[j],
      Births = births_this_year[j], Deaths = deaths_this_year[j],
      NetMigration = net_migration_vector[j], 
      HealthcareAccessScore = current_districts_status_df$HealthcareAccessScore[j], 
      EconomicOpportunityScore = current_districts_status_df$EconomicOpportunityScore[j] 
    )
    results_log_df <- rbind(results_log_df, log_row)
  }
} 
# Section 5.3: After the Loop Completion Message
message("--- Forward Projection Simulation loop complete. Detailed results stored in results_log_df. ---")
# --- End of V. Simulation Loop ---

# --- VI. Results Output ---
message("--- Preparing Results Output (from Forward Projection) ---")

# Section 6.1: Print Head and Tail of Detailed Log
message("Displaying summary of projection results_log_df:")
print("First few rows of results_log_df:")
print(head(results_log_df))
print("Last few rows of results_log_df:")
print(tail(results_log_df))

# Section 6.2: Population Trajectories Plot
message("Generating population trajectories plot for projection results...")
if (suppressWarnings(require(ggplot2, quietly = TRUE))) {
    message("ggplot2 package is available. Generating plot using ggplot2...")
    plot_title <- "Population Projection by District (ggplot2) - V2 Simulation"
    results_log_df$DistrictID <- factor(results_log_df$DistrictID)
    population_plot <- ggplot(results_log_df, aes(x = Year, y = Population, group = DistrictID, color = DistrictID)) +
                       geom_line(linewidth = 1) + 
                       labs(title = plot_title, x = "Projection Year", y = "Population (count)", color = "District ID") +
                       theme_minimal(base_size = 14) +
                       theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                             legend.title = element_text(face = "bold"), legend.position = "top" )
    print(population_plot) 
    message("ggplot2 plot for projection generated and displayed.")
} else {
    message("ggplot2 package not available. Generating plot using Base R graphics for projection results.")
    message("For a potentially more aesthetically pleasing plot, consider installing ggplot2: install.packages('ggplot2')")
    unique_districts_sim <- levels(factor(results_log_df$DistrictID)) 
    base_plot_colors_sim <- c("blue", "red", "green3", "purple", "orange", "brown", "pink", "grey20")
    if (length(unique_districts_sim) > length(base_plot_colors_sim)) {
        warning("More districts than predefined base colors for simulation plot. Colors will be recycled.")
        plot_colors_sim <- rep(base_plot_colors_sim, length.out = length(unique_districts_sim))
    } else {
        plot_colors_sim <- base_plot_colors_sim[1:length(unique_districts_sim)]
    }
    y_min_sim <- if(nrow(results_log_df) > 0) min(results_log_df$Population, na.rm = TRUE) else 0
    y_max_sim <- if(nrow(results_log_df) > 0) max(results_log_df$Population, na.rm = TRUE) else 1
    if (nrow(results_log_df) > 0 && !all(is.na(results_log_df$Population))) {
        y_padding_sim <- (y_max_sim - y_min_sim) * 0.05 
        y_limits_sim <- c(y_min_sim - y_padding_sim, y_max_sim + y_padding_sim)
        y_limits_sim[1] <- max(0, y_limits_sim[1])
    } else { 
        y_limits_sim <- c(0,1) 
    }
    if (length(unique_districts_sim) > 0 && nrow(results_log_df) > 0) {
        first_district_data_sim <- subset(results_log_df, DistrictID == unique_districts_sim[1])
        plot(first_district_data_sim$Year, first_district_data_sim$Population, type = "l", col = plot_colors_sim[1],
             lwd = 2, ylim = y_limits_sim, xlab = "Projection Year", ylab = "Population (count)",
             main = "Population Projection by District (Base R) - V2 Simulation")
        if (length(unique_districts_sim) > 1) {
            for (d_idx in 2:length(unique_districts_sim)) {
                district_data_sim <- subset(results_log_df, DistrictID == unique_districts_sim[d_idx])
                lines(district_data_sim$Year, district_data_sim$Population, type = "l", col = plot_colors_sim[d_idx], lwd = 2)
            }
        }
        legend("topright", legend = unique_districts_sim, col = plot_colors_sim, lty = 1, lwd = 2, cex = 0.8, 
               title = "District ID", box.lty = 0) 
        message("Base R plot for projection generated and displayed.")
    } else {
        message("No data available in results_log_df to generate Base R plot for projection.")
    }
}
# --- End of VI. Results Output ---

# --- VII. Documentation and Caveats ---
# Purpose: To provide additional context, explanations, and limitations of the V2 model.
message("--- Section VII: Documentation and Caveats ---")
# Key Changes in V2:
# 1. Simulated Historical Data: Section I generates a synthetic 'historical' dataset with known
#    underlying parameters and noise. This is a placeholder for real-world data.
# 2. Coefficient Estimation: Section II uses linear regression (lm()) on the simulated
#    historical data to 'estimate' the impact of structural indicators (HealthcareAccessScore,
#    EconomicOpportunityScore) on fertility and mortality rates. It also estimates
#    district-specific base rates.
# 3. Parameter Derivation for Simulation: Section III (formerly V1's Section I) now uses these
#    statistically estimated coefficients and base rates to parameterize the forward-looking
#    simulation model. Initial populations and indicator scores for the simulation period
#    are also set in this section.
# 4. Simulation Logic: Sections IV (Core Functions) and V (Simulation Loop) use these
#    derived parameters. The fundamental simulation mechanics (how rates are calculated,
#    how migration occurs, how policies are applied) remain similar to V1 but now operate
#    on parameters informed by a (simulated) data-driven estimation step.
# Important Caveats and Limitations:
# - Data is Simulated: The 'historical data' and 'estimated coefficients' are based on a
#   simulation with known true parameters. In a real application, historical data would be
#   messy, incomplete, and the true underlying relationships unknown. Statistical modeling
#   would be far more complex, involving variable selection, model diagnostics, validation,
#   and addressing issues like multicollinearity, autocorrelation, and endogeneity.
# - Model Simplifications:
#   - Linearity: The lm() assumes linear relationships between indicators and demographic rates.
#     Real-world relationships are often non-linear.
#   - Independence: Standard lm() assumes independence of errors. Time-series and panel data
#     often have correlated errors. More advanced statistical techniques (e.g., panel
#     data models, time series models) would be necessary for robust estimation.
#   - Omitted Variable Bias: If important variables influencing demographic rates are
#     omitted from the regression models, the estimated coefficients for included variables
#     may be biased.
#   - Structural Stability: Assumes coefficients estimated from 'historical' data remain
#     stable for the projection period. This may not hold true.
# - Migration Model: The migration model parameters (weights, base propensity) are still
#   assumed and not estimated from data in V2. A complete data-driven approach would
#   also model migration based on historical patterns and relevant drivers.
# - Policy Effects: Policy impacts are implemented as direct value changes to indicators,
#   similar to V1. Estimating policy impact functions from data is a complex task.
# - Exogenous Indicators: Structural indicators (HealthcareAccessScore, EconomicOpportunityScore)
#   are modified by policies but are otherwise not dynamic within the simulation loop (i.e., they
#   don't evolve based on population changes or other factors within the model itself, unless
#   a policy changes them). A more advanced model might make these endogenous.
# Purpose of V2:
# This version is primarily an educational tool to illustrate the CONCEPTUAL FLOW of:
#   Data -> Statistical Model -> Parameter Estimation -> Simulation Model Parameterization -> Projection.
# It highlights how a simulation model can be grounded in (even if simulated) empirical data.
# It is NOT intended to be a fully robust or validated forecasting model for real-world use
# without substantial enhancements to data inputs and statistical rigor.
message("--- GhanaMultiDistrictPopulationModel_V2 script execution finished. ---")
>>>>>>> REPLACE
