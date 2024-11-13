
# Runs the baseline model on the untreated observations
# Return the dataset with the residuals (married_resid)

run_first_stage <- function(all_dat) {
  
  # Run the model for the counterfactual
  first_stage <- feols(married ~ 1 |
                         cohort[age_turned, rural] + year_iso[age_turned, rural],
                       data=subset(all_dat, drought2==0),
                       vcov=~Adm1, weights=~Denorm_Wt)
  
  # Calculate the residuals
  all_dat$married_resid <- 
    all_dat$married - predict(first_stage, newdata = all_dat)
  
  # Return the dataframe with the residuals
  all_dat
}

