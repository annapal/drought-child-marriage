
# Create table information for the appendix

make_table_a1 <- function(all_dat) {
  
  # Dataframe to store the table
  tab_a1 <- data.frame()
  
  for (i in unique(all_dat$iso)) {
    data <- subset(all_dat, iso==i) # Get data for speciic country
    country <- countrycode(i, "iso3c", "un.name.en") # Country name
    
    # Get included surveys
    surveys <- sub(".*(\\d{4})DHS", "DHS \\1", unique(data$surveyid))
    surveys <- sub(".*(MICS\\d+)", "\\1", surveys)
    surveys <- paste0(surveys, collapse = ", ")
    
    # Get drought events
    events <- as.vector(na.omit(unique(data$event_no)))
    years <- as.numeric(substr(events, 1, 4))
    events <- events[order(years)]
    events <- paste0(events, collapse = ", ")
    
    years <- paste0(min(data$year), "-", max(data$year)) # Years included
    marriages <- sum(data$married) # Number of child marriages
    pt <- nrow(data) # Number of person years
    
    # Return the data
    tab_a1 <- rbind(tab_a1, c(country, surveys, events, years, marriages, pt))
  }
  
  # Save the table
  colnames(tab_a1) <- c("country", "surveys", "events", "years", "marriages", "person-time")
  write_xlsx(tab_a1, "results/tab_a1.xlsx")
}
