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

# GDIS data at Adm2 level
gdis_all_adm2 <- read_excel("data/emdat/gdis_adm2.xlsx")

# ISOs of countries where Adm2 analysis will be performed
adm2_countries <- drought_dat_all %>% filter(`Adm2 Analysis`=="Yes") %>% select(ISO)

# Create data frames to store results -------------------------------------

results_all <- data.frame() # Main results
results_res <- data.frame() # Results by rural/urban status
results_time <- data.frame() # Results over time
unit_trends <- data.frame() # Coefficients for unit-level trends
results_lt <- data.frame() # Results with unit-level linear time trend
results_adm2 <- data.frame() # Results using Adm2 level regions

prop_misclass <- data.frame() # Proportion of obs misclassified
prop_country <- data.frame() # Annual probability of marriage by country
prop_region <- data.frame() # Annual probability of marriage by region
table_a1 <- data.frame() # Table A1
table_a2 <- data.frame() # Table A2

# List to store drought panel data (need this for the map)
drought_panel_dat <- list()

# List of countries included in the analysis
countries <- unique(drought_dat_all$ISO)

# Run analysis for each country -------------------------------------------

for (iso in countries) {

  # Create the drought panel data
  drought_dat <- create_drought_panel(iso, drought_dat_all, gdis_all)
  drought_panel_dat[[iso]] <- drought_dat # Store the panel data for later
  
  # Read in the DHS-MICS data
  data <- readRDS(paste0("data/dhs-mics/", iso, ".rds"))
  
  # Merge DHS-MICS data with drought data
  data_merged <- merge_drought_dat(drought_dat, data)
  
  # Run the ETWFE model
  mod <- run_model(drought_dat, data_merged)
  
  # Calculate marginal treatment effects
  result_list <- calculate_te(mod, data_merged)
  
  # Store the results
  results_all <- rbind(results_all, result_list[[1]])
  results_res <- rbind(results_res, result_list[[2]])
  results_time <- rbind(results_time, result_list[[3]])
  
  # Run PT test
  pt_list <- run_pt_test(data_merged, drought_dat)
  
  # Store the results
  unit_trends <- rbind(unit_trends, pt_list[[1]]) # Coefficients for LT
  results_lt <- rbind(results_lt, pt_list[[2]]) # Results when including LT
  
  # Run the analysis at the Adm2 level (if applicable)
  if (iso %in% adm2_countries$ISO) {
    
    # Create drought panels
    drought_dat_adm2 <- create_drought_panel_adm2(iso, drought_dat_all, gdis_all_adm2)
    
    # Run the analysis
    results2 <- run_adm2(drought_dat_adm2, data)
    results_adm2 <- rbind(results_adm2, results2) # Store the results
  }
  
  # Calculate the proportion of observations misclassified (where applicable)
  prop_mc <- calc_misclassified(data_merged, drought_dat)
  prop_misclass <- rbind(prop_misclass, prop_mc) # Store the results
  
  # Calculate the average annual probability of marriage in each region
  prop <- avg_prob_region(data_merged)
  prop_region <- rbind(prop_region, prop) # Store the results
  
  # Calculate the average annual probability of marriage in the country
  prop2 <- avg_prob_country(data_merged)
  prop_country <- rbind(prop_country, prop2) # Store the results
  
  # Get data for Table A1
  tab1 <- make_table_a1(iso, data_merged)
  table_a1 <- rbind(table_a1, tab1) # Store the results
  
  # Get data for Table A2
  tab2 <- make_table_a2(iso, data_merged)
  table_a2 <- rbind(table_a2, tab2) # Store the results
}

# Save the results --------------------------------------------------------

# If the results folder doesn't exist, create it
if (!dir.exists("results")) {
  dir.create("results")
}

write_xlsx(results_all, "results/all_tes.xlsx")
write_xlsx(results_res, "results/tes_by_rural.xlsx")
write_xlsx(results_time, "results/tes_by_yr.xlsx")

write_xlsx(unit_trends, "results/linear_unit_trends.xlsx")
write_xlsx(results_lt, "results/tes_with_lt.xlsx")
write_xlsx(results_adm2, "results/all_tes_adm2.xlsx")

colnames(prop_misclass) <- c("country", "n_total", "n_move", "prop")
write_xlsx(prop_misclass, "results/misclassification.xlsx")

write_xlsx(prop_country, "results/prop_country.xlsx")
write_xlsx(prop_region, "results/prop_region.xlsx")

colnames(table_a1) <- c("Country", "Included surveys", "Included drought events")
write_xlsx(table_a1, "results/table_a1.xlsx")

colnames(table_a2) <- c("Country", "Years included", "No. of child marriages", "No. of person-years")
write_xlsx(table_a2, "results/table_a2.xlsx")

save(drought_panel_dat, file="data/emdat/drought_panel_dat.RData")

# Make figures ------------------------------------------------------------

# If the figures folder doesn't exist, create it
if (!dir.exists("figures")) {
  dir.create("figures")
}

# Make maps
plot_prob_map(prop_region)
plot_drought_map(drought_panel_dat)

# Plot proportion of observations misclassified
plot_misclass(prop_misclass)

# Plot the main results
plot_main(results_all)

# Plot the effects by rural/urban status
plot_rural(results_res)

# Plot the effects over consecutive years of drought exposure
plot_time(results_time)

# Plot results of the PT test
plot_pt_coefs(unit_trends)
plot_lt(results_lt, results_all)

# Plot results of Adm2 analysis
plot_adm2(results_adm2, results_all)


