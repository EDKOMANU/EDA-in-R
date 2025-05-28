# --- Regional Structural Population Model ---
# This R script implements a two-region (A and B) population projection model.
# It simulates population changes based on births, deaths, and migration.
# Migration is influenced by structural factors (economic opportunity, housing quality)
# for each region. The model also allows for a one-time policy intervention
# that can affect one of the structural factors in a chosen region.
# Results are stored in a data frame and a basic population plot is generated.

# --- I. Global Parameters and Data Structures ---

# Section 1: Initial Populations
population_A <- 10000  # Initial population (number of individuals) for Region A
population_B <- 8000   # Initial population (number of individuals) for Region B

# Section 2: Demographic Rates (Annual)
birth_rate_A <- 0.015  # Crude birth rate for Region A (e.g., 15 births per 1000 people per year)
birth_rate_B <- 0.012  # Crude birth rate for Region B (e.g., 12 births per 1000 people per year)
death_rate_A <- 0.008  # Crude death rate for Region A (e.g., 8 deaths per 1000 people per year)
death_rate_B <- 0.007  # Crude death rate for Region B (e.g., 7 deaths per 1000 people per year)

# Section 3: Structural Factors
# These are scores, typically on a scale of 0 to 100, representing regional attractiveness.
economic_opportunity_A <- 60  # Initial economic opportunity score for Region A (0-100 scale)
economic_opportunity_B <- 75  # Initial economic opportunity score for Region B (0-100 scale)
housing_quality_A <- 70       # Initial housing quality score for Region A (0-100 scale)
housing_quality_B <- 60       # Initial housing quality score for Region B (0-100 scale)

# Section 4: Migration Parameters
base_migration_rate <- 0.01   # Base annual migration rate (proportion of population that might move if max desirability difference exists)
econ_weight <- 0.6            # Weight for economic opportunity in desirability score (0.0 to 1.0)
housing_weight <- 0.4         # Weight for housing quality in desirability score (0.0 to 1.0)
                               # Note: econ_weight and housing_weight should ideally sum to 1.0 if used as direct proportions.

# Section 5: Policy Parameters
policy_intervention_year <- 10    # The simulation year the policy takes effect (integer)
policy_region <- "A"              # The region targeted by the policy ("A" or "B")
policy_structural_factor <- "economic_opportunity" # Factor affected ("economic_opportunity" or "housing_quality")
policy_effect_multiplier <- 1.20  # Multiplier for the policy effect (e.g., 1.20 for a 20% increase, 0.80 for 20% decrease)

# Section 6: Simulation Time
simulation_years <- 50  # Total number of years to simulate (integer)

# Section 7: Results Storage
# An empty data frame to store simulation results year by year.
results_df <- data.frame(
  Year = integer(simulation_years),                   # Simulation year
  Population_A = numeric(simulation_years),           # Population of Region A
  Population_B = numeric(simulation_years),           # Population of Region B
  Economic_Opportunity_A = numeric(simulation_years), # Economic score for Region A
  Economic_Opportunity_B = numeric(simulation_years), # Economic score for Region B
  Housing_Quality_A = numeric(simulation_years),      # Housing score for Region A
  Housing_Quality_B = numeric(simulation_years),      # Housing score for Region B
  Net_Migration_A_to_B = numeric(simulation_years)    # Net migrants from A to B (positive if A->B, negative if B->A)
)

# Optional: Initial status print (can be uncommented for debugging)
# print("Model parameters and data structures initialized.")
# print(paste("Initial Population A:", population_A))
# print(paste("Initial Population B:", population_B))
# print(paste("Simulation Years:", simulation_years))
# print("Results DataFrame structure:")
# print(str(results_df))

# --- II. Core Model Functions ---

# Function to calculate births in a region for one year.
# Args:
#   population (numeric): Current population of the region.
#   birth_rate (numeric): Crude birth rate of the region (annual).
# Returns:
#   numeric: Estimated number of births for the year.
calculate_births <- function(population, birth_rate) {
  births <- population * birth_rate
  return(births)
}

# Function to calculate deaths in a region for one year.
# Args:
#   population (numeric): Current population of the region.
#   death_rate (numeric): Crude death rate of the region (annual).
# Returns:
#   numeric: Estimated number of deaths for the year.
calculate_deaths <- function(population, death_rate) {
  deaths <- population * death_rate
  return(deaths)
}

# Function to calculate net migration between Region A and Region B for one year.
# Migration is driven by differences in weighted desirability scores.
# Args:
#   pop_A (numeric): Current population of Region A.
#   pop_B (numeric): Current population of Region B.
#   econ_A (numeric): Economic opportunity score for Region A (0-100).
#   econ_B (numeric): Economic opportunity score for Region B (0-100).
#   housing_A (numeric): Housing quality score for Region A (0-100).
#   housing_B (numeric): Housing quality score for Region B (0-100).
#   base_migration_rate (numeric): Base annual migration propensity.
#   econ_weight (numeric): Weight for economic opportunity in desirability.
#   housing_weight (numeric): Weight for housing quality in desirability.
# Returns:
#   numeric: Net number of migrants from Region A to Region B.
#            Positive value means net flow A -> B.
#            Negative value means net flow B -> A.
calculate_migration <- function(pop_A, pop_B, econ_A, econ_B, housing_A, housing_B, 
                                base_migration_rate, econ_weight, housing_weight) {
  
  # Calculate overall desirability scores for each region.
  # These scores are a weighted sum of the structural factors.
  desirability_A <- (econ_A * econ_weight) + (housing_A * housing_weight)
  desirability_B <- (econ_B * econ_weight) + (housing_B * housing_weight)
  
  # Calculate attractiveness difference.
  # Normalized to a range of -1 to 1 (assuming structural factors are 0-100 and weights sum to 1).
  # Positive value means A is more desirable; negative means B is more desirable.
  attractiveness_difference <- (desirability_A - desirability_B) / 100 
  
  net_migration_A_to_B <- 0 # Initialize net migration
  
  # Determine direction and magnitude of migration based on attractiveness difference.
  if (attractiveness_difference > 0) {
    # A is more attractive, potential flow from B to A.
    # Net migration from A to B will be negative (representing flow from B to A).
    # Migration is proportional to the attractiveness difference and the population of the source region (B).
    net_migration_A_to_B <- -1 * attractiveness_difference * pop_B * base_migration_rate
  } else if (attractiveness_difference < 0) {
    # B is more attractive, potential flow from A to B.
    # Net migration from A to B will be positive (representing flow from A to B).
    # attractiveness_difference is negative, so -1 * (negative value) makes the result positive.
    # Migration is proportional to the absolute attractiveness difference and the population of the source region (A).
    net_migration_A_to_B <- -1 * attractiveness_difference * pop_A * base_migration_rate
  }
  
  # Note on model robustness:
  # This is a simplified migration model. A more complex model might include:
  # - Caps on migration (e.g., not exceeding a certain percentage of the source population).
  # - Non-linear responses to desirability differences.
  # - Effects of distance or existing migrant networks.
  # For current purposes, parameters are assumed to be set to avoid unrealistic outcomes (e.g., negative population).
  
  return(net_migration_A_to_B)
}

# Function to apply a one-time policy effect on structural factors in a specific year.
# The policy modifies a chosen structural factor in a chosen region by a given multiplier.
# Args:
#   current_year (integer): The current simulation year.
#   current_econ_A (numeric): Current economic opportunity score for Region A.
#   current_econ_B (numeric): Current economic opportunity score for Region B.
#   current_housing_A (numeric): Current housing quality score for Region A.
#   current_housing_B (numeric): Current housing quality score for Region B.
#   policy_year (integer): The year the policy intervention is scheduled to occur.
#   policy_reg (character): The region targeted by the policy ("A" or "B").
#   policy_factor (character): The structural factor to be affected 
#                              (e.g., "economic_opportunity", "housing_quality").
#   policy_multiplier (numeric): The multiplier to apply to the affected factor (e.g., 1.1 for +10%).
# Returns:
#   list: A list containing the structural factors, updated if the policy was applied this year:
#         list(econ_A, econ_B, housing_A, housing_B).
apply_policy_effect <- function(current_year, current_econ_A, current_econ_B, 
                                current_housing_A, current_housing_B, 
                                policy_year, policy_reg, policy_factor, policy_multiplier) {
  
  # Create copies of current structural factors to return; these will be modified if policy applies.
  updated_econ_A <- current_econ_A
  updated_econ_B <- current_econ_B
  updated_housing_A <- current_housing_A
  updated_housing_B <- current_housing_B

  # Apply the policy only if the current simulation year is the policy intervention year.
  if (current_year == policy_year) {
    message(paste0("--- Policy Intervention Triggered in Year ", current_year, " ---"))
    if (policy_reg == "A") {
      if (policy_factor == "economic_opportunity") {
        updated_econ_A <- current_econ_A * policy_multiplier
        message(paste0("Economic opportunity in Region A changed by a factor of ", policy_multiplier, 
                       ". Old: ", round(current_econ_A,1), ", New: ", round(updated_econ_A,1)))
      } else if (policy_factor == "housing_quality") {
        updated_housing_A <- current_housing_A * policy_multiplier
        message(paste0("Housing quality in Region A changed by a factor of ", policy_multiplier,
                       ". Old: ", round(current_housing_A,1), ", New: ", round(updated_housing_A,1)))
      }
      # Extend with 'else if' for other factors in Region A if the model grows.
    } else if (policy_reg == "B") {
      if (policy_factor == "economic_opportunity") {
        updated_econ_B <- current_econ_B * policy_multiplier
        message(paste0("Economic opportunity in Region B changed by a factor of ", policy_multiplier,
                       ". Old: ", round(current_econ_B,1), ", New: ", round(updated_econ_B,1)))
      } else if (policy_factor == "housing_quality") {
        updated_housing_B <- current_housing_B * policy_multiplier
        message(paste0("Housing quality in Region B changed by a factor of ", policy_multiplier,
                       ". Old: ", round(current_housing_B,1), ", New: ", round(updated_housing_B,1)))
      }
      # Extend with 'else if' for other factors in Region B if the model grows.
    }
    # Extend with 'else if' for other regions if the model expands.
  }
  
  return(list(econ_A = updated_econ_A, econ_B = updated_econ_B, 
              housing_A = updated_housing_A, housing_B = updated_housing_B))
}

# --- III. Simulation Loop ---
print("Starting simulation...")

# Section 1: Initialize Current State Variables
# These variables will be updated each year of the simulation.
# They start with the globally defined initial values.
current_pop_A <- population_A
current_pop_B <- population_B
current_econ_A <- economic_opportunity_A
current_econ_B <- economic_opportunity_B
current_housing_A <- housing_quality_A
current_housing_B <- housing_quality_B

# Section 2: Loop through Simulation Years
for (year in 1:simulation_years) {
  
  # Step a: Apply Policy Effects (if any for the current year)
  # This function checks if it's the policy year and modifies factors accordingly.
  policy_updates <- apply_policy_effect(
    current_year = year,
    current_econ_A = current_econ_A,
    current_econ_B = current_econ_B,
    current_housing_A = current_housing_A,
    current_housing_B = current_housing_B,
    policy_year = policy_intervention_year,
    policy_reg = policy_region,
    policy_factor = policy_structural_factor,
    policy_multiplier = policy_effect_multiplier
  )
  # Update the current structural factors with the (potentially modified) values.
  current_econ_A <- policy_updates$econ_A
  current_econ_B <- policy_updates$econ_B
  current_housing_A <- policy_updates$housing_A
  current_housing_B <- policy_updates$housing_B
  
  # Step b: Calculate Natural Increase (Births and Deaths)
  births_A <- calculate_births(current_pop_A, birth_rate_A)
  deaths_A <- calculate_deaths(current_pop_A, death_rate_A)
  births_B <- calculate_births(current_pop_B, birth_rate_B)
  deaths_B <- calculate_deaths(current_pop_B, death_rate_B)
  
  # Step c: Update Populations due to Natural Increase
  current_pop_A <- current_pop_A + births_A - deaths_A
  current_pop_B <- current_pop_B + births_B - deaths_B
  # Population floor: ensure population doesn't drop below zero.
  current_pop_A <- max(0, current_pop_A)
  current_pop_B <- max(0, current_pop_B)
  
  # Step d: Calculate Migration
  net_migration_A_to_B <- calculate_migration(
    pop_A = current_pop_A,
    pop_B = current_pop_B,
    econ_A = current_econ_A,
    econ_B = current_econ_B,
    housing_A = current_housing_A,
    housing_B = current_housing_B,
    base_migration_rate = base_migration_rate,
    econ_weight = econ_weight,
    housing_weight = housing_weight
  )
  
  # Step e: Update Populations due to Migration
  # Adjust population of A: decrease if A->B (positive net_migration), increase if B->A (negative net_migration)
  current_pop_A <- current_pop_A - net_migration_A_to_B 
  # Adjust population of B: increase if A->B, decrease if B->A
  current_pop_B <- current_pop_B + net_migration_A_to_B 
  # Population floor and migration cap (simple version):
  # Ensure populations don't go negative due to migration.
  # A more robust model might cap net_migration_A_to_B to not exceed the source population.
  current_pop_A <- max(0, current_pop_A)
  current_pop_B <- max(0, current_pop_B)
  
  # Step f: Store Results for the Current Year
  # Add a new row to the results_df data frame.
  results_df[year, ] <- list(
    year,
    current_pop_A,
    current_pop_B,
    current_econ_A,
    current_econ_B,
    current_housing_A,
    current_housing_B,
    net_migration_A_to_B
  )
} # End of simulation loop for 'year'

# --- IV. Post-Simulation Output ---
print("Simulation complete. Results stored in results_df.")

# Section 1: Display Head and Tail of the Results Data Frame
print("First few rows of results_df:")
print(head(results_df))

print("Last few rows of results_df:")
print(tail(results_df))

# Section 2: Generate Basic Population Plot
print("Generating a simple population plot...")

# Determine a suitable y-axis range to encompass both population trajectories, ignoring any NA values.
y_axis_min <- min(c(results_df$Population_A, results_df$Population_B), na.rm = TRUE)
y_axis_max <- max(c(results_df$Population_A, results_df$Population_B), na.rm = TRUE)
y_axis_range <- c(y_axis_min, y_axis_max)


# Create the plot for Population A.
plot(results_df$Year, results_df$Population_A, type = "l", col = "blue", 
     ylim = y_axis_range, # Set y-axis limits
     xlab = "Year", ylab = "Population", 
     main = "Population Projection: Region A vs Region B")

# Add the line for Population B to the existing plot.
lines(results_df$Year, results_df$Population_B, type = "l", col = "red")

# Add a legend to distinguish the lines.
legend("topright", legend = c("Population A", "Population B"), 
       col = c("blue", "red"), lty = 1, cex = 0.8) # lty=line type, cex=character expansion factor

# --- End of Script ---
print("Script execution finished.")
