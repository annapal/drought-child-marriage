
run_model <- function(drought_dat, data) {
  
  # Run extended twfe model
  start_yr <- min(drought_dat$year)
  mod <- feols(married ~ drought2:i(cohort, i.year, ref=1, ref2=start_yr)/
                 (age_dm + rural_dm)|
                 cohort[age_turned, rural] + year[age_turned, rural],
               data=data, vcov=~Adm1, weights=~Denorm_Wt,
               mem.clean=TRUE, notes = FALSE)
  
  # Return the model object
  mod
}