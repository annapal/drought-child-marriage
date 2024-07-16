
# Make tables for the appendix

make_tables <- function(data_merged_drought) {

  # Datasets and events -----------------------------------------------------
  
  table_a1 <- data.frame() # Table to store the data
  
  for (iso in names(data_merged_drought)) {
  
    data <- data_merged_drought[[iso]] # Get the data
    country <- countrycode(iso, "iso3c", "un.name.en") # Country
    
    # Get included surveys
    surveys <- sub(".*(\\d{4})DHS", "DHS \\1", unique(data$surveyid))
    surveys <- sub(".*(MICS\\d+)", "\\1", surveys)
    surveys <- paste0(surveys, collapse = ", ")
    
    # Get drought events
    events <- as.vector(na.omit(unique(data$event_no)))
    years <- as.numeric(substr(events, 1, 4))
    events <- events[order(years)]
    events <- paste0(events, collapse = ", ")
    
    # Add to the table
    table_a1 <- rbind(table_a1, c(country, surveys, events))
  }
  
  # Save the table
  colnames(table_a1) <- c("Country", "Included surveys", "Included drought events")
  write_xlsx(table_a1, "results/table_a1.xlsx")
  print("Table A1 saved in: results/table_a1.xlsx")
  
  # Years and person-time ---------------------------------------------------
  
  table_a2 <- data.frame() # Table to store the data
  
  for (iso in names(data_merged_drought)) {
    
    data <- data_merged_drought[[iso]] # Get the data
    country <- countrycode(iso, "iso3c", "un.name.en") # Country
    years <- paste0(min(data$year), "-", max(data$year)) # Years included
    marriages <- sum(data$married) # Number of child marriages
    pt <- nrow(data) # Number of person years
    
    # Add to the table
    table_a2 <- rbind(table_a2, c(country, years, marriages, pt))
  }
  
  # Save the table
  colnames(table_a2) <- c("Country", "Years included", "No. of child marriages", "No. of person-years")
  write_xlsx(table_a2, "results/table_a2.xlsx")
  print("Table A2 saved in: results/table_a2.xlsx")
}
