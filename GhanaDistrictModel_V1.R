# --- Ghana District Population Model V1 ---
#
# File Purpose:
# This R script implements Version 1 of a multi-district population projection model,
# notionally for districts in Ghana. It simulates demographic changes over time,
# incorporating dynamic fertility and mortality rates influenced by structural factors,
# inter-district migration based on relative attractiveness, and the impact of
# pre-defined policy interventions on structural indicators.
#
# Components:
# 1. Parameter Initialization: Defines initial district characteristics, demographic
#    coefficients, migration weights, policy settings, and simulation duration.
# 2. Core Functions:
#    - Dynamic Demographic Rates: Calculates year-specific fertility and mortality.
#    - Migration: Determines inter-district migration flows.
#    - Policy Application: Modifies structural indicators based on scheduled policies.
# 3. Simulation Loop: Iterates through years, applying functions to update district
#    populations and indicators.
# 4. Results Output: Logs annual data and generates basic visualizations.
#
# Current Scope (V1):
# - Uses sample data for 3 hypothetical districts.
# - Incorporates 2 structural indicators (HealthcareAccessScore, EconomicOpportunityScore).
# - Implements a simplified policy mechanism (direct value change).
#
# Long-Term Vision:
# Designed with future scalability in mind to handle a larger number of districts
# (e.g., all 261 districts of Ghana), allow user-defined structural indicators to be
# read from external data files, and potentially model more complex demographic
# and socio-economic relationships.
#
# --- I. Global Parameters and Data Structures ---

# Section 1.1: Main District Data Frame
# This data frame holds the static and initial dynamic characteristics of each district.
districts_df <- data.frame(
  DistrictID = factor(c("D1", "D2", "D3")),    # Unique identifier for each district (factor)
  InitialPopulation = c(50000, 70000, 30000),  # Population count at the start of the simulation (numeric)
  BaseFertilityRate = c(0.035, 0.040, 0.030),  # Base per capita annual fertility rate (numeric, e.g., births per person per year)
  BaseMortalityRate = c(0.010, 0.008, 0.012),  # Base per capita annual mortality rate (numeric, e.g., deaths per person per year)
  HealthcareAccessScore = c(60, 75, 50),      # Score (0-100) representing healthcare access quality/availability (numeric)
  EconomicOpportunityScore = c(50, 80, 40)   # Score (0-100) representing economic opportunity level (numeric)
)
# SCALABILITY NOTE: This data frame can be expanded to include all 261 districts.
# Additional columns for more structural indicators can also be added. The rest of the
# script logic (e.g., loops, apply functions) is designed to adapt to the number of
# rows/indicators if column names are used consistently. For example, reading this
# from a CSV file would be a common next step for a larger dataset.

# Section 1.2: Coefficients for Dynamic Demographic Rates
# These coefficients determine how structural factors (scores) influence fertility and mortality.
# They represent the change in the respective demographic rate per unit point of the score.
# For V1, these are defined globally. In future versions, they could be district-specific or vary over time.

# Effect of HealthcareAccessScore on fertility.
coeff_fert_health_effect <- 0.0001 # Expected: Higher health score -> lower fertility. (Numeric: rate decrease per score point)

# Effect of EconomicOpportunityScore on fertility.
coeff_fert_econ_effect <- 0.0002   # Expected: Higher econ score -> lower fertility. (Numeric: rate decrease per score point)

# Effect of HealthcareAccessScore on mortality.
coeff_mort_health_effect <- 0.00015 # Expected: Higher health score -> lower mortality. (Numeric: rate decrease per score point)

# Effect of EconomicOpportunityScore on mortality.
coeff_mort_econ_effect <- 0.0001   # Expected: Higher econ score -> lower mortality. (Numeric: rate decrease per score point)

# Section 1.3: Migration Model Parameters
# These parameters control how migration flows are calculated based on district attractiveness.

# Weight for HealthcareAccessScore in calculating a district's attractiveness for migration.
migration_health_weight <- 0.4 # Numeric (0.0 to 1.0)

# Weight for EconomicOpportunityScore in calculating a district's attractiveness for migration.
migration_econ_weight <- 0.6   # Numeric (0.0 to 1.0)
# Note: For the current attractiveness calculation, these weights should ideally sum to 1.0
# if they are to represent direct proportions of the scores in a simple weighted sum.

# General scaling factor for overall migration propensity between districts.
base_migration_propensity <- 0.005 # Numeric. Represents the fraction of a district's population that
                                   # would consider moving per unit of normalized attractiveness difference (0-100 scale for difference).
                                   # E.g., if diff=10, 10 * 0.005 = 5% of source pop migrates.

# Section 1.4: Simulation Time
# Defines the duration of the simulation.
simulation_years <- 30  # Number of years to simulate (integer)

# Section 1.5: Pre-defined Policy List
# This list defines policy interventions that occur at specific years for specific districts.
# Each policy is a list containing:
#   - year: The simulation year the policy takes effect (integer).
#   - district_id: The DistrictID (factor or character) targeted by the policy.
#   - indicator_name: The column name (character) in the districts' status data frame to be changed
#                     (e.g., "HealthcareAccessScore", "EconomicOpportunityScore").
#   - new_value: The direct new value (numeric) to set for the indicator.
# For V1, policies provide a direct new value. Future versions could use multipliers or additive effects.

policy1 <- list(
  year = 10,
  district_id = "D1",
  indicator_name = "HealthcareAccessScore", 
  new_value = 85 
)

policy2 <- list(
  year = 15,
  district_id = "D2",
  indicator_name = "EconomicOpportunityScore",
  new_value = 90
)

# Combine all defined policies into a single list.
policies_list <- list(policy1, policy2) 
# SCALABILITY NOTE: Policy definitions could be read from an external file (e.g., CSV, JSON)
# or be more dynamically generated (e.g., through a UI or scenario builder) in a scaled-up version.

# Section 1.6: Results Storage Data Frame
# This data frame will store the state of each district for each year of the simulation.
# It is initialized as an empty data frame with predefined column names and types.
results_log_df <- data.frame(
  Year = integer(),                      # Simulation year (integer)
  DistrictID = factor(),                 # District identifier (factor)
  Population = numeric(),                # Population count at the end of the year (numeric)
  DynamicFertilityRate = numeric(),      # Calculated fertility rate for the year (numeric)
  DynamicMortalityRate = numeric(),      # Calculated mortality rate for the year (numeric)
  Births = numeric(),                    # Total births in the year (numeric)
  Deaths = numeric(),                    # Total deaths in the year (numeric)
  NetMigration = numeric(),              # Net migration count for the district in the year (numeric)
  HealthcareAccessScore = numeric(),     # Healthcare score at the end of the year (numeric, 0-100)
  EconomicOpportunityScore = numeric()   # Economic score at the end of the year (numeric, 0-100)
)
# Note: If the script is run multiple times in an interactive session, this line
# re-initializes results_log_df, effectively clearing previous run's results.

# --- End of I. Global Parameters and Data Structures ---

# --- II. Core Model Functions ---

# Section 2.1: Dynamic Demographic Rate Functions

# Function to calculate the dynamic fertility rate for a district.
# This rate is adjusted from a base rate based on current structural factor scores
# (healthcare and economic opportunity) and their respective impact coefficients.
# Args:
#   base_fertility (numeric): The district's base (initial) fertility rate.
#   health_score (numeric): The district's current HealthcareAccessScore (0-100).
#   econ_score (numeric): The district's current EconomicOpportunityScore (0-100).
#   c_fert_h_effect (numeric): Global coefficient for health score's effect on fertility.
#                              (Positive value implies rate decreases as score increases).
#   c_fert_e_effect (numeric): Global coefficient for econ score's effect on fertility.
#                              (Positive value implies rate decreases as score increases).
# Returns:
#   numeric: The calculated dynamic fertility rate (per capita, annual), ensured non-negative.
calculate_dynamic_fertility_rate <- function(base_fertility, health_score, econ_score, 
                                             c_fert_h_effect, c_fert_e_effect) {
  # Calculate the total adjustment based on health and economic scores multiplied by their effects.
  fertility_adjustment <- (health_score * c_fert_h_effect) + (econ_score * c_fert_e_effect)
  
  # Adjust the base rate. The effect coefficients determine direction (subtraction implies inverse relationship).
  dynamic_rate <- base_fertility - fertility_adjustment
  
  # Ensure the fertility rate does not fall below zero.
  dynamic_rate <- max(0, dynamic_rate)
  
  return(dynamic_rate)
}

# Function to calculate the dynamic mortality rate for a district.
# Similar to fertility, this rate is adjusted from a base rate based on structural factors.
# Args:
#   base_mortality (numeric): The district's base (initial) mortality rate.
#   health_score (numeric): The district's current HealthcareAccessScore (0-100).
#   econ_score (numeric): The district's current EconomicOpportunityScore (0-100).
#   c_mort_h_effect (numeric): Global coefficient for health score's effect on mortality.
#                              (Positive value implies rate decreases as score increases).
#   c_mort_e_effect (numeric): Global coefficient for econ score's effect on mortality.
#                              (Positive value implies rate decreases as score increases).
# Returns:
#   numeric: The calculated dynamic mortality rate (per capita, annual), ensured non-negative.
calculate_dynamic_mortality_rate <- function(base_mortality, health_score, econ_score, 
                                              c_mort_h_effect, c_mort_e_effect) {
  # Calculate the total adjustment based on health and economic scores.
  mortality_adjustment <- (health_score * c_mort_h_effect) + (econ_score * c_mort_e_effect)
  
  # Adjust the base rate.
  dynamic_rate <- base_mortality - mortality_adjustment
  
  # Ensure the mortality rate does not fall below zero.
  dynamic_rate <- max(0, dynamic_rate)
  
  return(dynamic_rate)
}

# Section 2.2: Migration Functions

# Function to calculate an attractiveness score for a single district.
# This score is a weighted sum of its structural factor scores.
# Args:
#   health_score (numeric): The district's current HealthcareAccessScore (0-100).
#   econ_score (numeric): The district's current EconomicOpportunityScore (0-100).
#   health_weight (numeric): Global weight for health score in attractiveness calculation.
#   econ_weight (numeric): Global weight for econ score in attractiveness calculation.
# Returns:
#   numeric: The calculated attractiveness score (typically 0-100 if weights sum to 1 and scores are 0-100).
calculate_district_attractiveness <- function(health_score, econ_score, health_weight, econ_weight) {
  # The scores are assumed to be on a 0-100 scale from input.
  # Weights are assumed to be normalized (e.g., sum to 1) for the resulting score to be easily interpretable.
  attractiveness <- (health_score * health_weight) + (econ_score * econ_weight)
  return(attractiveness)
}

# Function to calculate net migration for all districts based on relative attractiveness.
# It implements a pairwise comparison: for each pair of districts (i, j), if j is more
# attractive than i, a flow from i to j is calculated. Net migration for each district
# is the sum of its inflows minus the sum of its outflows.
# Args:
#   current_districts_data (data.frame): Data frame of current district status, must include
#                                       'DistrictID', 'Population', 'HealthcareAccessScore', 
#                                       'EconomicOpportunityScore'.
#   health_w (numeric): Global weight for health score in attractiveness.
#   econ_w (numeric): Global weight for econ score in attractiveness.
#   base_propensity (numeric): Global base migration propensity factor.
# Returns:
#   numeric vector: A vector of net migration counts for each district, in the same order as
#                   rows in `current_districts_data`. Positive values mean net inflow, negative mean net outflow.
calculate_interdistrict_migration <- function(current_districts_data, health_w, econ_w, base_propensity) {
  
  num_districts <- nrow(current_districts_data)
  
  # 1. Calculate Attractiveness for all Districts
  # Create a temporary working data frame to hold attractiveness and current population.
  mig_calc_df <- current_districts_data 
  mig_calc_df$Attractiveness <- numeric(num_districts)
  
  for (k in 1:num_districts) {
    mig_calc_df$Attractiveness[k] <- calculate_district_attractiveness(
      health_score = mig_calc_df$HealthcareAccessScore[k],
      econ_score = mig_calc_df$EconomicOpportunityScore[k],
      health_weight = health_w,
      econ_weight = econ_w
    )
  }
  
  # 2. Initialize Net Migration Vector
  # This vector will store the net migration for each district.
  net_migration_final <- numeric(num_districts)
  names(net_migration_final) <- mig_calc_df$DistrictID # For clarity, not strictly needed for calculation

  # 3. Calculate Pairwise Migration Flows
  # Iterate through all possible pairs of origin (i) and destination (j) districts.
  # SCALABILITY NOTE: The current pairwise calculation of flows is O(N^2) where N is number of districts.
  # For a very large N (e.g., 261 districts), performance might be a concern (261^2 is ~68k pairs).
  # Optimized matrix operations or alternative migration modeling approaches (e.g., gravity models,
  # batched calculations) might be explored for substantial performance gains in scaled-up versions.
  for (i in 1:num_districts) {
    for (j in 1:num_districts) {
      if (i == j) {
        next # Skip migration from a district to itself
      }
      
      # Calculate attractiveness difference: positive if j is more attractive than i.
      attractiveness_difference_j_vs_i <- mig_calc_df$Attractiveness[j] - mig_calc_df$Attractiveness[i]
      
      if (attractiveness_difference_j_vs_i > 0) { # If district j is more attractive than district i
        # Potential flow from district i to district j.
        # Flow is proportional to the attractiveness difference, the origin population, and base propensity.
        # The attractiveness_difference is on a 0-100 scale.
        flow_i_to_j <- attractiveness_difference_j_vs_i * mig_calc_df$Population[i] * base_propensity
        
        # Cap the flow: e.g., at most 50% of origin district's population can flow to any single other district.
        # This prevents unrealistic depletion in one step to one destination.
        flow_i_to_j <- min(flow_i_to_j, mig_calc_df$Population[i] * 0.50)
        
        # Update net migration: district i loses population, district j gains population.
        # This method ensures population is conserved across all districts (sum of net_migration_final is zero).
        net_migration_final[i] <- net_migration_final[i] - flow_i_to_j 
        net_migration_final[j] <- net_migration_final[j] + flow_i_to_j 
      }
    }
  }
  
  return(net_migration_final)
}

# Section 2.3: Policy Function

# Function to apply scheduled policies to the districts' structural indicators.
# Policies are defined in `policies_list` and are triggered if a policy's defined
# year matches the current simulation year.
# Args:
#   current_districts_status_df (data.frame): Data frame with current district status (DistrictID, 
#                                             HealthcareAccessScore, EconomicOpportunityScore, Population etc.).
#                                             This data frame is copied, and the copy is modified and returned.
#   list_of_policies (list): A list of policy definitions. Each policy is a list containing
#                            `year`, `district_id`, `indicator_name`, and `new_value`.
#   current_sim_year (integer): The current year in the simulation.
# Returns:
#   data.frame: The `updated_districts_df` with policy effects applied for the current year.
#               If no policies apply, the original data frame is returned.
apply_district_policy <- function(current_districts_status_df, list_of_policies, current_sim_year) {
  
  updated_districts_df <- current_districts_status_df # Make a copy to modify and return
  
  for (policy in list_of_policies) {
    if (policy$year == current_sim_year) {
      # Find the row index for the district specified in the policy.
      # Using `as.character` for matching in case DistrictID is a factor.
      row_index <- which(as.character(updated_districts_df$DistrictID) == as.character(policy$district_id))
      
      if (length(row_index) == 1) { # Ensure exactly one district is found.
        indicator_to_change <- policy$indicator_name
        new_indicator_value <- policy$new_value
        
        # Check if the indicator_name actually exists as a column before trying to update.
        if (indicator_to_change %in% names(updated_districts_df)) {
          old_value <- updated_districts_df[row_index, indicator_to_change]
          
          # Update the value in the specified row and column.
          updated_districts_df[row_index, indicator_to_change] <- new_indicator_value
          
          message(paste0("Policy Applied in Year ", current_sim_year, 
                         ": District ", policy$district_id, 
                         "'s '", indicator_to_change, 
                         "' changed from ", round(old_value, 2),
                         " to ", round(new_indicator_value, 2), "."))
        } else {
          # Warning if the specified indicator column doesn't exist.
          warning(paste0("Policy Warning in Year ", current_sim_year, 
                         ": Indicator '", indicator_to_change, 
                         "' not found in districts data frame for District ", policy$district_id, ". Policy not applied."))
        }
      } else if (length(row_index) == 0) {
        # Warning if the target district ID is not found.
        warning(paste0("Policy Warning in Year ", current_sim_year, 
                       ": District ID '", policy$district_id, "' not found. Policy not applied."))
      } else {
        # Warning if multiple matches for the district ID are found (should not happen with unique IDs).
        warning(paste0("Policy Warning in Year ", current_sim_year, 
                       ": Multiple matches for District ID '", policy$district_id, "' found. Policy not applied."))
      }
    }
  }
  
  return(updated_districts_df)
}
# --- End of II. Core Model Functions ---

# --- III. Simulation Loop ---
message("--- Starting Simulation ---") # Changed print to message for consistency

# Section 3.1: Initialize Current State for Simulation
# Create a working copy of the district data. This data frame will be updated each year.
# It needs to include columns for all indicators that might be affected by policies
# and those needed for calculations (Population, scores, base rates).
current_districts_status_df <- districts_df[, c("DistrictID", "BaseFertilityRate", "BaseMortalityRate", "HealthcareAccessScore", "EconomicOpportunityScore")]
current_districts_status_df$Population <- districts_df$InitialPopulation # Use 'Population' for current year's population.

num_all_districts <- nrow(current_districts_status_df)

# Section 3.2: Main Loop through Simulation Years
for (year in 1:simulation_years) {
  message(paste("--- Simulating Year:", year, "of", simulation_years, "---"))

  # Step A: Apply Policy Changes
  # Policies modify structural indicators (e.g., HealthcareAccessScore) for the current year.
  current_districts_status_df <- apply_district_policy(
    current_districts_status_df = current_districts_status_df,
    list_of_policies = policies_list,
    current_sim_year = year
  )
  
  # Step B: Calculate Demographics and Update Population (Natural Increase)
  # These calculations are done district by district.
  # Initialize vectors to store this year's calculated values for logging.
  current_dynamic_fertility_rates <- numeric(num_all_districts)
  current_dynamic_mortality_rates <- numeric(num_all_districts)
  births_this_year <- numeric(num_all_districts)
  deaths_this_year <- numeric(num_all_districts)
  
  for (i in 1:num_all_districts) {
    district_data_row <- current_districts_status_df[i, ]
    
    # Calculate dynamic demographic rates for the current district.
    dynamic_fert_rate <- calculate_dynamic_fertility_rate(
      base_fertility = district_data_row$BaseFertilityRate,
      health_score = district_data_row$HealthcareAccessScore,
      econ_score = district_data_row$EconomicOpportunityScore,
      c_fert_h_effect = coeff_fert_health_effect,
      c_fert_e_effect = coeff_fert_econ_effect
    )
    current_dynamic_fertility_rates[i] <- dynamic_fert_rate # Store for logging
    
    dynamic_mort_rate <- calculate_dynamic_mortality_rate(
      base_mortality = district_data_row$BaseMortalityRate,
      health_score = district_data_row$HealthcareAccessScore,
      econ_score = district_data_row$EconomicOpportunityScore,
      c_mort_h_effect = coeff_mort_health_effect,
      c_mort_e_effect = coeff_mort_econ_effect
    )
    current_dynamic_mortality_rates[i] <- dynamic_mort_rate # Store for logging
    
    # Calculate absolute number of births and deaths.
    # Population used is from the start of the year (before this year's natural increase or migration).
    num_births <- district_data_row$Population * dynamic_fert_rate
    births_this_year[i] <- num_births # Store for logging
    
    num_deaths <- district_data_row$Population * dynamic_mort_rate
    deaths_this_year[i] <- num_deaths # Store for logging
    
    # Update population based on natural increase (births - deaths).
    current_districts_status_df$Population[i] <- current_districts_status_df$Population[i] + num_births - num_deaths
    current_districts_status_df$Population[i] <- max(0, current_districts_status_df$Population[i]) # Ensure population is not negative.
  }
  
  # Step C: Calculate Inter-District Migration
  # This function returns a vector of net migrants for each district.
  # Population data used by this function is after natural increase but before migration adjustment for the year.
  net_migration_vector <- calculate_interdistrict_migration(
    current_districts_data = current_districts_status_df, 
    health_w = migration_health_weight,
    econ_w = migration_econ_weight,
    base_propensity = base_migration_propensity
  )
  
  # Step D: Update Populations due to Migration
  # Apply the net migration to each district's population.
  for (i in 1:num_all_districts) {
    current_districts_status_df$Population[i] <- current_districts_status_df$Population[i] + net_migration_vector[i]
    current_districts_status_df$Population[i] <- max(0, current_districts_status_df$Population[i]) # Ensure population is not negative.
  }
  
  # Step E: Log Results for the Current Year
  # For each district, create a row with all relevant data for the year and append to results_log_df.
  for (j in 1:num_all_districts) {
    log_row <- data.frame(
      Year = year,
      DistrictID = current_districts_status_df$DistrictID[j],
      Population = current_districts_status_df$Population[j], # Population at end of year
      DynamicFertilityRate = current_dynamic_fertility_rates[j],
      DynamicMortalityRate = current_dynamic_mortality_rates[j],
      Births = births_this_year[j],
      Deaths = deaths_this_year[j],
      NetMigration = net_migration_vector[j], 
      HealthcareAccessScore = current_districts_status_df$HealthcareAccessScore[j], # Score as of end of year (after policy)
      EconomicOpportunityScore = current_districts_status_df$EconomicOpportunityScore[j] # Score as of end of year
    )
    
    # Append the log row to the main results data frame.
    # SCALABILITY NOTE: For a large number of districts and simulation_years, repeatedly using
    # rbind() in a loop can be inefficient due to repeated memory reallocations. Consider
    # pre-allocating the results_log_df with NA values and filling rows by index, or using
    # a list of data frames and then a single dplyr::bind_rows() or data.table::rbindlist()
    # call after the loop for significantly better performance in scaled-up versions.
    results_log_df <- rbind(results_log_df, log_row)
  }
  
} # End of simulation loop for 'year'

# Section 3.3: After the Loop Completion Message
message("--- Simulation loop complete. Detailed results stored in results_log_df. ---")
# --- End of III. Simulation Loop ---

# --- IV. Results Output ---
message("--- Preparing Results Output ---")

# Section 4.1: Print Head and Tail of Detailed Log
message("Displaying summary of results_log_df:")
print("First few rows of results_log_df:")
print(head(results_log_df))

print("Last few rows of results_log_df:")
print(tail(results_log_df))

# Section 4.2: Population Trajectories Plot
message("Generating population trajectories plot...")

# Attempt to use ggplot2 for plotting; if not available, use base R.
if (suppressWarnings(require(ggplot2, quietly = TRUE))) {
    message("ggplot2 package is available. Generating plot using ggplot2...")
    plot_title <- "Population Projection by District (ggplot2)"
    
    # Ensure DistrictID in results_log_df is treated as a factor for correct color mapping and legend.
    results_log_df$DistrictID <- factor(results_log_df$DistrictID)
    
    population_plot <- ggplot(results_log_df, aes(x = Year, y = Population, group = DistrictID, color = DistrictID)) +
                       geom_line(linewidth = 1) + 
                       labs(title = plot_title, x = "Year", y = "Population (count)", color = "District ID") +
                       theme_minimal(base_size = 14) +
                       theme(
                         plot.title = element_text(hjust = 0.5, face = "bold"),
                         legend.title = element_text(face = "bold"),
                         legend.position = "top" # More conventional position for legends
                       )
    print(population_plot) # Display the plot
    message("ggplot2 plot generated and displayed.")
    
} else {
    message("ggplot2 package not available. Generating plot using Base R graphics.")
    message("For a potentially more aesthetically pleasing plot, consider installing ggplot2: install.packages('ggplot2')")
    
    unique_districts <- levels(factor(results_log_df$DistrictID)) # Use levels of factor for defined order
    
    # Define a palette of colors for base R plotting.
    base_plot_colors <- c("blue", "red", "green3", "purple", "orange", "brown", "pink", "grey20")
    if (length(unique_districts) > length(base_plot_colors)) {
        warning("More districts than predefined base colors. Colors will be recycled.")
        plot_colors <- rep(base_plot_colors, length.out = length(unique_districts))
    } else {
        plot_colors <- base_plot_colors[1:length(unique_districts)]
    }

    # Determine overall y-axis limits to accommodate all district populations.
    y_min <- min(results_log_df$Population, na.rm = TRUE)
    y_max <- max(results_log_df$Population, na.rm = TRUE)
    y_padding <- (y_max - y_min) * 0.05 # Add 5% padding
    y_limits <- c(y_min - y_padding, y_max + y_padding)
    y_limits[1] <- max(0, y_limits[1]) # Ensure y-axis doesn't go below 0

    # Create the initial plot for the first district.
    if (length(unique_districts) > 0) {
        first_district_data <- subset(results_log_df, DistrictID == unique_districts[1])
        plot(first_district_data$Year, first_district_data$Population, type = "l", col = plot_colors[1],
             lwd = 2, ylim = y_limits, xlab = "Year", ylab = "Population (count)",
             main = "Population Projection by District (Base R)")

        # Add lines for other districts to the same plot.
        if (length(unique_districts) > 1) {
            for (d_idx in 2:length(unique_districts)) {
                district_data <- subset(results_log_df, DistrictID == unique_districts[d_idx])
                lines(district_data$Year, district_data$Population, type = "l", col = plot_colors[d_idx], lwd = 2)
            }
        }

        # Add a legend to the plot.
        legend("topright", legend = unique_districts, col = plot_colors, lty = 1, lwd = 2, cex = 0.8, 
               title = "District ID", box.lty = 0) # box.lty=0 for no box around legend
        message("Base R plot generated and displayed.")
    } else {
        message("No data available in results_log_df to generate Base R plot.")
    }
}
# --- End of IV. Results Output ---

# --- End of GhanaDistrictModel_V1 Script ---
message("--- GhanaDistrictModel_V1 script execution finished. ---")
