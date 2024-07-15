## Load packages
source("./packages.R")

## Load R files
# lapply(list.files("./R", full.names = TRUE, recursive = TRUE), source)

# Run the analysis --------------------------------------------------------

# Read in the DHS/MCIS data
# Processed in separate git repo: ... 
load("data/clean_data_long.RData")

# Generate drought panel data and plot the panels
drought_panel_dat <- create_drought_panels()

# Run TWFE models for all countries
# WARNING: Takes a long time to run, and requires a lot of memory
twfe_data <- run_models(long_data, drought_panel_dat)

# Calculate marginal treatment effects for all countries
# WARNING: Takes a long time to run, and requires a lot of memory
calculate_tes(twfe_data$all_models, twfe_data$data_merged_drought)

# Run test for parallel trends
# WARNING: Takes a long time to run, and requires a lot of memory
pt_test(twfe_data$data_merged_drought, drought_panel_dat)

# Create figures and tables for paper ------------------------------------

# Map of annual probability of marriage among included countries
plot_prob_map(data_merged_drought)

# Map of approximate location of drought events
plot_drought_map(drought_panel_dat)

# Make tables for appendix
# TODO




