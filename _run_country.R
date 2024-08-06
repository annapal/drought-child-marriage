## Load packages
source("./packages.R")

## Load R files
lapply(list.files("./R", full.names = TRUE, recursive = TRUE), source)

# Run the analysis --------------------------------------------------------

# sep_files()

# Read in edited drought data from spreadsheet
drought_dat_all <- suppressWarnings(read_excel("data/emdat/emdat_drought_events_updated.xlsx")) %>%
  filter(Include=="Yes")

# Read in GDIS data to get regions
gdis_all <- suppressWarnings(read_excel("data/emdat/pend-gdis-1960-2018-disasterlocations_updated.xlsx"))

# Create dataframes to store the results
results_all <- data.frame() # Main results
results_res <- data.frame() # Results by rural/urban status
results_time <- data.frame() # Results over time
results_adm2 <- data.frame() # Results for adm2 analysis

# Countries where Adm2 analysis will be performed
adm2_countries <- drought_dat_all %>% filter(`Adm2 Analysis`=="Yes")

iso <- "MDG"

# Create the drought panel data
drought_dat <- create_drought_panel(iso, drought_dat_all, gdis_all)

# Read in the DHS-MICS data
data <- readRDS(paste0("data/dhs-mics/", iso, ".rds"))

# Merge the data with the drought data
data_merged <- merge_drought_dat(drought_dat, data)
mod <- run_model(drought_dat, data_merged)

# Calculate treatment effects
result_list <- calculate_te(mod, data_merged)

# Store the results
results_all <- rbind(results_all, result_list[[1]])
results_res <- rbind(results_res, result_list[[2]])
results_time <- rbind(results_time, result_list[[3]])

# Run the analysis at the Adm2 level (if applicable)
if (iso %in% adm2_countries$ISO) {
  
  # Read in GDIS data at Adm2 level
  gdis_all_adm2 <- read_excel("data/emdat/gdis_adm2.xlsx")
  
  # Create drought panels
  drought_dat_adm2 <- create_drought_panel_adm2(iso, drought_dat_all, gdis_all_adm2)
  
  # Run the analysis
  results2 <- run_adm2(drought_dat_adm2, data)
  results_adm2 <- rbind(results_adm2, results2) # Store the results
}


