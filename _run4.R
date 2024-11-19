## Load packages
source("./packages.R")

## Load R files
lapply(list.files("./R", full.names = TRUE, recursive = TRUE), source)

# Prepare the data
# all_dat <- combine_data()
all_dat <- readRDS("data/all_dat.rds")

# Calculate the average probability of marriage
prop_country <- avg_prob_country(all_dat)
prop_region <- avg_prob_region(all_dat)

# Calculate the proportion of py exposed to droughts
prop_drought <- prop_py_exposed(all_dat)

# Run the model
# run_model(all_dat)
results_all <- read_xlsx("results/etwfe_main.xlsx")
results_rural <- read_xlsx("results/etwfe_rural.xlsx")
results_3yr <- read_xlsx("results/etwfe_3yr.xlsx")

# Plot results ------------------------------------------------------------

# Figure 1
plot_prob_map(prop_region)
# plot_drought_map()

# Figure 2
plot_main(results_all, prop_country, prop_drought)

