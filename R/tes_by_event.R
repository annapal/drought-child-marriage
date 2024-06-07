
# Calculate the marginal treatment effects by drought event

# Load the DHS/MICS data and the etwfe models
load("data/data_merged_drought.Rdata")
load("data/all_models.Rdata")

# Iso codes of countries to include in the analysis
iso_cdes <- names(all_models)

# Create dataframes to store the results
results_all <- data.frame()

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
    by = "event_no2",
    wts = "Denorm_Wt"
  )
  result$iso <- iso
  results_all <- rbind(results_all, result) # Store results
  
  print(paste0(iso, " finished"))
  
}

write_xlsx(results_all, "results/tes_by_event.xlsx")

