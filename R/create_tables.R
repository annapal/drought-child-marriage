
# Function that gets table information for the appendix for specific country

# Table A1

make_table_a1 <- function(iso, data_merged) {
  
  country <- countrycode(iso, "iso3c", "un.name.en") # Country
  
  # Get included surveys
  surveys <- sub(".*(\\d{4})DHS", "DHS \\1", unique(data_merged$surveyid))
  surveys <- sub(".*(MICS\\d+)", "\\1", surveys)
  surveys <- paste0(surveys, collapse = ", ")
  
  # Get drought events
  events <- as.vector(na.omit(unique(data_merged$event_no)))
  years <- as.numeric(substr(events, 1, 4))
  events <- events[order(years)]
  events <- paste0(events, collapse = ", ")
  
  # Return the data
  c(country, surveys, events)
}

# Table A2

make_table_a2 <- function(iso, data_merged) {
  
  country <- countrycode(iso, "iso3c", "un.name.en") # Country
  years <- paste0(min(data_merged$year), "-", max(data_merged$year)) # Years included
  marriages <- sum(data_merged$married) # Number of child marriages
  pt <- nrow(data_merged) # Number of person years
  
  # Return the data
  c(country, years, marriages, pt)
}
