
# Run analysis with a unit-level linear trend to test PT assumption

run_pt_test <- function(data_merged, drought_dat) {
  
  # Run extended twfe model with unit-level linear time trend
  start_yr <- min(drought_dat$year)
  mod_lt <- feols(married ~ drought2:i(cohort, i.year, ref=1, ref2=start_yr)/
                    (age_dm + rural_dm) + 
                    i(cohort, year, ref=1)| # Add cohort-level linear trend
                    cohort[age_turned, rural] + year[age_turned, rural], 
                  data=data_merged, vcov=~Adm1, weights=~Denorm_Wt,
                  mem.clean = TRUE, notes = FALSE)
  
  # Extract coefficients for unit-level linear trends
  res_pt <- cbind(mod_lt$coefficients, confint(mod_lt), mod_lt$coeftable[,4])
  colnames(res_pt) <- c("coef", "lower", "upper", "p")
  res_pt2 <- res_pt[1:(max(data_merged$cohort)-1),]
  res_pt2$iso <- iso
  res_pt2$cohort <- 2:(max(data_merged$cohort))
  rownames(res_pt2) <- NULL
  
  # Get TEs when linear time trend is added
  result <- slopes(
    mod_lt, 
    newdata = subset(data_merged, drought2==1), # Only region-years exposed to drought
    variables = "drought2",
    by = "drought2",
    wts = "Denorm_Wt"
  )
  result$iso <- iso
  
  # Return results
  list(res_pt2, result)
}


