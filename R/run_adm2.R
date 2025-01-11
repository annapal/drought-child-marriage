
# Function that runs Adm2 analysis

run_adm2 <- function() {
  
  # EM-DAT drought data
  drought_dat_all <- suppressWarnings(read_excel("data/emdat/emdat_drought_events_updated.xlsx")) %>%
    filter(Include=="Yes")
  
  # GDIS data 
  gdis_all_adm2 <- suppressWarnings(read_excel("data/emdat/gdis_adm2.xlsx"))
  
  # Dataframe to store the results
  results_adm2 <- data.frame()
  
  for (i in unique(gdis_all_adm2$iso3)) {
    
    # Read in the DHS-MICS data
    data <- readRDS(paste0("data/dhs-mics/", i, ".rds"))
    
    # Get drought panel data at Adm2 level
    drought_dat_adm2 <- create_drought_panel_adm2(i, drought_dat_all, gdis_all_adm2)
    
    # Merge drought data to marriage data & remove non-relevant years
    data <- data %>% select(-Reg) # Remove variable Reg, so they can be joined properly
    data <- suppressMessages(left_join(data, drought_dat_adm2) %>% filter(!is.na(cohort)))
    
    # De-mean age and rural-urban status
    data$rural <- ifelse(data$res=="rural", 1, 0) # Make indicator for rural
    data<- data %>%
      group_by(cohort, year) %>% # De-mean within each cohort and year
      mutate(age_dm = age_turned - mean(age_turned),
             rural_dm = rural - mean(rural)) %>%
      ungroup()
    
    # Run extended twfe model
    start_yr <- min(drought_dat_adm2$year)
    mod <- feols(married ~ drought2:i(cohort, i.year, ref=1, ref2=start_yr)/
                   (age_dm + rural_dm)|
                   cohort[age_turned, rural] + year[age_turned, rural], 
                 data=data, vcov=~Adm2, weights=~Denorm_Wt,
                 notes = FALSE, mem.clean = TRUE)
    
    # Calculate main TE
    result <- slopes(
      mod,
      newdata = subset(data, drought2==1), # Only region-years exposed to drought
      variables = "drought2",
      by = "drought2",
      wts = "Denorm_Wt"
    )
    
    # Return result
    result$iso <- i
    results_adm2 <- rbind(result, results_adm2)
  }
  
  results_adm2 <- as.data.frame(results_adm2)
  write_xlsx(results_adm2, "results/etwfe_adm2.xlsx")
}
