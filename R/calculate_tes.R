
# Calculate the marginal treatment effects

calculate_tes <- function(all_models, data_merged_drought) {

  # Iso codes of countries to include in the analysis
  iso_cdes <- names(all_models)
  
  # Create dataframes to store the results
  results_all <- data.frame() # Main results
  results_res <- data.frame() # Results by rural/urban status
  results_time <- data.frame() # Results over time
  
  # Calculate TEs for each country
  for (iso in iso_cdes) {
    # Get the model
    mod <- all_models[[iso]]
    
    # Get the data
    data <- data_merged_drought[[iso]]
    
    # Calculate main TE
    result <- slopes(
      mod,
      newdata = subset(data, drought2==1), # Only region-years exposed to drought
      variables = "drought2",
      by = "drought2",
      wts = "Denorm_Wt"
    )
    result$iso <- iso # Append iso code
    results_all <- rbind(results_all, result) # Store results
    
    # Calculate TE by rural-urban status
    result2 <- slopes(
      mod, 
      newdata = subset(data, drought2==1), # Only region-years exposed to drought
      variables = "drought2",
      by = "rural",
      wts = "Denorm_Wt"
    )
    result2$iso <- iso # Append iso code
    results_res <- rbind(results_res, result2) # Store results
    
    # Calculate TE by years of consecutive exposure to drought
    if (nrow(subset(data, drought_3yr==1))>0) {
      result3 <- slopes(
        mod, 
        newdata = subset(data, drought_3yr==1), # Only region-years exposed to 3 years of drought
        variables = "drought2",
        by = "drought_yr",
        wts = "Denorm_Wt"
      )
      result3$iso <- iso # Append iso code
      results_time <- rbind(results_time, result3) # Store results
    }
    
  }
  
  # If the results folder doesn't exist, create it
  if (!dir.exists("results")) {
    dir.create("results")
  }
  
  # Write the results to excel
  write_xlsx(results_all, "results/all_tes.xlsx")
  write_xlsx(results_res, "results/tes_by_rural.xlsx")
  write_xlsx(results_time, "results/tes_by_yr.xlsx")
  
  print("Treatment effects saved in: results/all_tes.xlsx")
  print("Treatment effects by rural status saved in: results/tes_by_rural.xlsx")
  print("Treatment effects by year saved in: results/tes_by_yr.xlsx")
}

