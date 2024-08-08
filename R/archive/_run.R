## Load packages
source("./packages.R")

## Load R files
lapply(list.files("./R", full.names = TRUE, recursive = TRUE), source)

# Run the analysis --------------------------------------------------------

# sep_files()

# Generate drought panel data and plot the panels
drought_panel_dat <- create_drought_panels()

# Run TWFE models for all countries
# WARNING: Takes a long time to run, and requires a lot of memory
twfe_data <- run_models(long_data, drought_panel_dat)
all_models <- twfe_data$all_models # Model data
data_merged_drought <- twfe_data$data_merged_drought # Merged drought data
rm(twfe_data) # To save memory

# Calculate marginal treatment effects for all countries
# WARNING: Takes a long time to run, and requires a lot of memory
calculate_tes(all_models, data_merged_drought)

# Remove model data to save memory
rm(all_models)

# Run analysis at Adm2 level
drought_panel_dat_2 <- create_drought_panels_adm2()
adm2_analysis(long_data, drought_panel_dat_2)

# Remove long_data to save memory
rm(long_data)

# Run test for parallel trends
# WARNING: Takes a long time to run, and requires a lot of memory
pt_test(data_merged_drought, drought_panel_dat)

# Create figures and tables for paper ------------------------------------

# Make maps
plot_prob_map(data_merged_drought) # Probability of marriage
plot_drought_map(drought_panel_dat) # Location of drought events

# Make tables for appendix
make_tables(data_merged_drought)

# Plot treatment effects
plot_main_te() # Main figure
plot_te_rural() # By rural-urban status
plot_te_time() # By consecutive years of drought exposure

# Plot PT tests
plot_pt_coefs() # Coefs of unit-level trends
plot_te_lt() # TEs with unit-level trends

# Plot sensitivity analyses
plot_te_adm2() # Adm2 analysis
calc_misclassification(data_merged_drought, drought_panel_dat)

