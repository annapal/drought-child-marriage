
# Run the ETWFE model at the Adm2 level

adm2_analysis <- function(long_data, drought_panel_dat) {

  # Create list to store models
  all_models <- list()
  
  # Create a list to store the DHS/MICS and drought merged data
  data_merged_drought <- list()
  
  # Iso codes of countries to include in the analysis
  iso_cdes <- names(drought_panel_dat)
  
  results_all <- data.frame() # Main results
  
  for (iso in iso_cdes) {
    
    # Get data for specific country
    data <- long_data[[iso]] %>% filter(!is.na(Adm2)) %>% select(-Reg)
    drought_dat <- drought_panel_dat[[iso]]
    
    # Assign year after drought ends as 1
    drought_dat <- drought_dat %>%
      arrange(Reg, year) %>%
      group_by(Reg) %>%
      mutate(
        drought2=ifelse(lag(drought, default=0)==1, 1, drought),
        lagged=ifelse((drought2-drought==1), 1, 0) # Indicator where drought exposure is lagged
      ) %>%
      ungroup()
    
    # Assign cohorts to the drought data based on treatment history
    drought_cohort <- drought_dat %>%
      group_by(Reg) %>%
      summarize(drought_sequence = paste(drought2, collapse = ""))
    drought_cohort$cohort <- as.numeric(factor(drought_cohort$drought_sequence))
    drought_dat <- suppressMessages(left_join(drought_dat, drought_cohort))
  
    # Merge drought data to marriage data & remove non-relevant years
    data <- suppressMessages(left_join(data, drought_dat) %>% filter(!is.na(cohort)))
    
    # De-mean age and rural-urban status
    data$rural <- ifelse(data$res=="rural", 1, 0) # Make indicator for rural
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
                 data=data, vcov=~Adm2, weights=~Denorm_Wt,
                 notes = FALSE, mem.clean = TRUE)
    
    # Save model to list
    all_models[[iso]] <- mod
    
    # Save the merged data to list
    data_merged_drought[[iso]] <- data
    
    # Calculate main TE
    result <- slopes(
      mod,
      newdata = subset(data, drought2==1), # Only region-years exposed to drought
      variables = "drought2",
      by = "drought2",
      wts = "Denorm_Wt"
    )
    result$iso <- iso
    results_all <- rbind(results_all, result) # Store results
    
  } 
  
  # Save the results
  save(all_models, file="data/all_models_adm2.Rdata")
  save(data_merged_drought, file="data/data_merged_drought_adm2.Rdata")
  write_xlsx(results_all, "results/all_tes_adm2.xlsx")
  print("Model data (Adm2) saved in: data/all_models_adm2.Rdata")
  print("Merged data (Adm2) saved in: data/data_merged_drought_adm2.Rdata")
  print("Treatment effects (Adm2) saved in: results/all_tes_adm2.xlsx")
}
  
