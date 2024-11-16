# Runs the baseline model with state linear trend

run_first_stage_lt <- function(all_dat) {
  
  # Run the model for the counterfactual
  first_stage <- feols(married ~ i(Adm1, year, ref=1) |
                         Adm1[age_turned, rural] + year_iso[age_turned, rural],
                       data=subset(all_dat, drought2==0),
                       vcov=~Adm1, weights=~Denorm_Wt)
  
  # Calculate the residuals
  all_dat$married_resid_lt <- 
    all_dat$married - predict(first_stage, newdata = all_dat)
  
  # Return the dataframe with the residuals
  all_dat
}
