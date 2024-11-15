# Produce results by consecutive year of drought

run_3yr <- function(all_dat) {
  
  # Dataframe to store the results
  results_3yr <- data.frame()
  
  # Countries
  countries <- unique(all_dat$iso3)
  
  # Create variable for regions exposed to 3 yrs of drought
  all_dat$drought_consec <- case_when(
    !is.na(all_dat$drought_yr) & all_dat$drought_yr < 3 & !is.na(all_dat$drought_3yr) ~ 
      all_dat$drought_yr * all_dat$drought_3yr + 1,
    all_dat$drought2 == 1 ~ 99,
    TRUE ~ 0
  )
  
  for (i in countries) {
    
    # Run the second OLS with the residuals
    second_stage <- feols(
      married_resid ~ i(drought_consec, ref = 0),
      data = subset(all_dat, iso3==i),
      vcov=~Adm1, weights=~Denorm_Wt
    )
    
    # Save the results
    results <- broom::tidy(second_stage, conf.int = TRUE) %>%
      filter(term %in% c("drought_consec::1", "drought_consec::2", "drought_consec::3"))
    results$iso <- i
    results$year <- as.numeric(substr(results$term, 17, 18))
    results_3yr <- rbind(results_3yr, results)
  }
  
  # Save the results
  write_xlsx(results_3yr, "results/3yr_tes.xlsx")
  
  # Return the results
  results_3yr
}
