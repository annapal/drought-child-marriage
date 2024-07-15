
# Run the ETWFE model for all countries

run_models <- function(long_data, drought_panel_dat) {
  
  # Create list to store model results
  all_models <- list()
  
  # Create a list to store the DHS/MICS and drought merged data
  data_merged_drought <- list()
  
  # Iso codes of countries to include in the analysis
  iso_cdes <- names(drought_panel_dat)
  
  # Progress bar
  pb <- txtProgressBar(min = 0, max = length(iso_cdes), style = 3)
  
  # Run the model for each country
  for (iso in iso_cdes) {
    
    # Get data for specific country
    data <- long_data[[iso]] %>% filter(!is.na(Adm1))
    drought_dat <- drought_panel_dat[[iso]]
    
    # Assign year after drought ends as 1
    drought_dat <- drought_dat %>%
      arrange(Adm1, year) %>%
      group_by(Adm1) %>%
      mutate(
        drought2=ifelse(lag(drought, default=0)==1, 1, drought), # New drought exposure variable
        lagged=ifelse((drought2-drought==1), 1, 0), # Indicator of when drought exposure is lagged
        event_no2 = ifelse(lagged==1, lag(event_no), event_no) # Event no. for new drought variable
      ) %>%
      ungroup()
    
    # Assign cohorts to the drought data based on treatment history
    drought_cohort <- drought_dat %>%
      group_by(Adm1) %>%
      summarize(drought_sequence = paste(drought2, collapse = ""))
    drought_cohort$cohort <- as.numeric(factor(drought_cohort$drought_sequence))
    drought_dat <- suppressMessages(left_join(drought_dat, drought_cohort))
    
    # Create variable showing consecutive years of exposure to drought
    drought_dat$drought_yr <- 0
    for (i in 2:nrow(drought_dat)) {
      if (drought_dat$drought2[i]==1 & drought_dat$drought2[i-1]==0) {
        drought_dat$drought_yr[i] <- 1 # First year of drought
      } else if (drought_dat$drought2[i]==1 & drought_dat$drought2[i-1]==1) {
        drought_dat$drought_yr[i] <- drought_dat$drought_yr[i-1] + 1 # Subsequent years of exposure
      }
    }
    
    # Create a variable for exposure to 3 years of drought
    # (i.e. all regions and years where there was at least 3 years of exposure)
    drought_dat$drought_3yr <- 0
    for (i in 3:nrow(drought_dat)) {
      if (drought_dat$drought_yr[i]==3) {
        drought_dat$drought_3yr[(i-2):i] <- 1
      }
    }
    
    # Merge drought data to marriage data & remove non-relevant years
    data <- suppressMessages(left_join(data, drought_dat) %>% filter(!is.na(cohort)))
    
    # De-mean age and rural-urban status
    data$rural <- ifelse(data$res=="rural", 1, 0) # Make indicator for rural status
    data<- data %>%
      group_by(cohort, year) %>% # De-mean within each cohort and year
      mutate(age_dm = age_turned - mean(age_turned),
             rural_dm = rural - mean(rural)) %>%
      ungroup()
  
    # Run extended twfe model
    start_yr <- min(drought_dat$year)
    mod <- feols(married ~ drought2:i(cohort, i.year, ref=1, ref2=start_yr)/
                   (age_dm + rural_dm)|
                   cohort[age_turned, rural] + year[age_turned, rural],
                 data=data, vcov=~Adm1, weights=~Denorm_Wt,
                 mem.clean=TRUE, notes = FALSE)
  
    # Save model to list
    all_models[[iso]] <- mod
    
    # Save the merged data to list
    data_merged_drought[[iso]] <- data
    
    # Update progress bar
    setTxtProgressBar(pb, iso)
    
  } 
  
  # Save all data
  save(all_models, file="data/all_models.Rdata")
  save(data_merged_drought, file="data/data_merged_drought.Rdata")
  print("Merged data saved in: data/data_merged_drought.Rdata")
  print("Model data saved in: data/all_models.Rdata")
  
  # Return model data and merged DHS/MICS-Drought data
  return(list(all_models, data_merged_drought))
}
