## Load packages
source("./packages.R")

## Load R files
lapply(list.files("./R", full.names = TRUE, recursive = TRUE), source)

# Load the data --------------------------------------------------------

# EM-DAT drought data
drought_dat_all <- suppressWarnings(read_excel("data/emdat/emdat_drought_events_updated.xlsx")) %>%
  filter(Include=="Yes")

# GDIS data 
gdis_all <- suppressWarnings(read_excel("data/emdat/pend-gdis-1960-2018-disasterlocations_updated.xlsx"))

# Prepare the data
all_dat <- combine_data(drought_dat_all, gdis_all)
# all_dat <- readRDS("data/all_dat.rds")

# Descriptive statistics --------------------------------------------------

# Calculate the average probability of marriage
prop_country <- avg_prob_country(all_dat)
prop_region <- avg_prob_region(all_dat)

# Calculate the proportion of py exposed to droughts
prop_drought <- prop_py_exposed(all_dat)

# Run the analysis -------------------------------------------------------

# Run the first stage
all_dat <- run_first_stage(all_dat)

# Main Results
results_all <- run_main(all_dat)

# Plot results ------------------------------------------------------------



# Figure 2
plot_main(results_all, prop_country, prop_drought)
  
