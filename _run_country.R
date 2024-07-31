## Load packages
source("./packages.R")

## Load R files
lapply(list.files("./R", full.names = TRUE, recursive = TRUE), source)

# Run the analysis --------------------------------------------------------

# sep_files()

# Read in edited drought data from spreadsheet
drought_dat <- suppressWarnings(read_excel("data/emdat/emdat_drought_events_updated.xlsx")) %>%
  filter(Include=="Yes")

# Read in GDIS data to get regions
gdis_all <- suppressWarnings(read_excel("data/emdat/pend-gdis-1960-2018-disasterlocations_updated.xlsx"))

# Create dataframes to store the results
results_all <- data.frame() # Main results
results_res <- data.frame() # Results by rural/urban status
results_time <- data.frame() # Results over time

iso <- "AFG"

# Create the drought panel data
drought_dat <- create_drought_panel(iso, drought_dat, gdis_all)

# Read in the DHS-MICS data
data <- readRDS(paste0("data/dhs-mics/", iso, ".rds"))

# Merge the data with the drought data
data <- merge_drought_dat(drought_dat, data)
mod <- run_model(drought_dat, data)

# Calculate treatment effects
result_list <- calculate_te(mod, data)

# Store the results
results_all <- rbind(results_all, result_list[[1]])
results_res <- rbind(results_res, result_list[[2]])
results_time <- rbind(results_time, result_list[[3]])



