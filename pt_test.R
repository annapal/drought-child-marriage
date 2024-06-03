
# Run analysis with a unit-level linear trend

# Load the DHS/MICS data and the etwfe models
load("data/data_merged_drought.Rdata")
load("data/emdat/drought_panel.Rdata")

all_lt_models <- list() # List to store models
unit_trends <- data.frame() # Dataframe to store coefficients for unit-level trends
results_lt <- data.frame() # Dataset to store TEs with unit-level linear time trend

# Iso codes of countries to include in the analysis
iso_cdes <- names(data_merged_drought)

for (iso in iso_cdes) {
  
  # Get the data
  data <- data_merged_drought[[iso]]
  drought_dat <- drought_panel_dat[[iso]]
  
  # Run extended twfe model with unit-level linear time trend
  start_yr <- min(drought_dat$year)
  mod_lt <- feols(married ~ drought2:i(cohort, i.year, ref=1, ref2=start_yr)/
                 (age_dm + rural_dm) + 
                   i(cohort, year, ref=1)| # Add cohort-level linear trend
                 cohort[age_turned, rural] + year[age_turned, rural], 
               data=data, vcov=~Adm1, weights=~Denorm_Wt)
  
  # Save model to list
  all_lt_models[[iso]] <- mod_lt
  
  # Extract coefficients for unit-level linear trends
  res_pt <- cbind(mod_lt$coefficients, confint(mod_lt), mod_lt$coeftable[,4])
  colnames(res_pt) <- c("coef", "lower", "upper", "p")
  res_pt2 <- res_pt[1:(max(data$cohort)-1),]
  res_pt2$iso <- iso
  res_pt2$cohort <- 2:(max(data$cohort))
  rownames(res_pt2) <- NULL
  
  # Add to dataframe
  unit_trends <- rbind(unit_trends, res_pt2)
  
  # Get TEs when linear time trend is added
  result <- slopes(
    mod_lt, 
    newdata = subset(data, drought2==1), # Only region-years exposed to drought
    variables = "drought2",
    by = "drought2",
    wts = "Denorm_Wt"
  )
  result$iso <- iso
  
  # Add results to the dataframe
  results_lt <- rbind(results_lt, result)
}

# Save the results
save(all_lt_models, file="data/all_lt_models.Rdata")
write_xlsx(unit_trends, "results/linear_unit_trends.xlsx")
write_xlsx(results_lt, "results/tes_with_lt.xlsx")




