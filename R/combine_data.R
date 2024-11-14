
# Combine all country datasets into one dataset

combine_data <- function(drought_dat_all, gdis_all) {
  
  # Data frame to store all the data
  all_dat <- data.frame()
  
  # List to store drought panel data (need this for the map)
  drought_panel_dat <- list()
  
  for (iso in unique(drought_dat_all$ISO)) {
    
    # Create the drought panel data
    drought_dat <- create_drought_panel(iso, drought_dat_all, gdis_all)
    drought_panel_dat[[iso]] <- drought_dat # Store the panel data for later
    
    # Read in the DHS-MICS data
    data <- readRDS(paste0("data/dhs-mics/", iso, ".rds"))
    
    # Merge DHS-MICS data with drought data
    data_merged <- suppressMessages(left_join(data, drought_dat) %>% filter(!is.na(cohort)))
    
    # Make indicator for rural status
    data_merged$rural <- ifelse(data_merged$res=="rural", 1, 0) 
    
    # Make cohorts & Adm1 unique to country
    data_merged$cohort <- paste(iso, data_merged$cohort, sep=": ")
    data_merged$Adm1 <- paste(iso, data_merged$Adm1, sep=": ")
    
    # Make year variable that is unique to country
    data_merged$year_iso <- paste(data_merged$year, iso, sep = "_")
    
    # Add to dataframe
    all_dat <- rbind(all_dat, data_merged)
  }
  
  # Save the dataframe
  saveRDS(all_dat, file="data/all_dat.rds")
  saveRDS(drought_panel_dat, file="data/drought_panel_dat.rds")
  
  # Return the dataframe
  all_dat
}
