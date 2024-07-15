## Load packages
source("./packages.R")

## Load R files
# lapply(list.files("./R", full.names = TRUE, recursive = TRUE), source)

# Read in the DHS/MCIS data
# Processed in separate git repo: ... 
load("data/clean_data_long.RData")

# Generate drought panel data and plot the panels
drought_panel_dat <- create_drought_panels()

# Run TWFE models for all countries
twfe_data <- run_models(long_data, drought_panel_dat)
