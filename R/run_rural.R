# Produce results by rural/urban regions

run_rural <- function(all_dat) {
  
  # Dataframe to store the results
  results_rural <- data.frame()
  
  # Countries
  countries <- unique(all_dat$iso3)
  
  for (i in countries) {
    
    # Run the second OLS with the residuals
    second_stage <- feols(
      married_resid ~ i(drought2, i.rural, ref = 0),
      data = subset(all_dat, iso3==i),
      vcov=~Adm1, weights=~Denorm_Wt
    )
    
    # Save the results
    results <- broom::tidy(second_stage, conf.int = TRUE)[2:3,]
    results$iso <- i
    results$rural <- c(0,1)
    results_rural <- rbind(results_rural, results)
  }
  
  # Save the results
  write_xlsx(results_rural, "results/rural_tes.xlsx")
  
  # Return the results
  results_rural
}
