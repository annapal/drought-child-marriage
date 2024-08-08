
# Save the list of data frames in `long_data` to separate files

sep_files <- function() {
  
  # Read in the DHS/MCIS data
  load("data/clean_data_long.RData")
  
  # Create a single file for each country
  dir.create("data/dhs-mics", showWarnings = FALSE)
  for (iso in names(long_data)) {
    saveRDS(long_data[[iso]], paste0("data/dhs-mics/", iso, ".rds"))
  }
  
  rm(long_data)
  gc()
}