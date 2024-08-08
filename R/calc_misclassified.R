
# Estimate the proportion of observations that are misclassified based on available data

calc_misclassified <- function(data_merged, drought_dat) {

  # Remove event number & drought plot
  drought_dat <- drought_dat %>%
    select(-event_no, -drought_plot)
  
  # Sample size for each country
  n <- nrow(data_merged)
  
  # Create moving variables
  data_merged <- data_merged %>%
    mutate(
      # Create variable indicating if woman has moved region
      moved = case_when(
        is.na(al_lived) ~ "Unknown", # Unknown if woman moved
        al_lived==1 ~ "No", # Woman always lived in same location
        al_lived==0 & age-yrs_lived<13 ~ "No", # Woman moved prior to being at risk of child marriage
        al_lived==0 & Adm1==Prev_Adm1 ~ "No", # Woman has moved, but moved within same region
        al_lived==0 & is.na(Prev_Adm1) ~ "Unknown", # Woman has moved, but to unknown region
        Adm1!=Prev_Adm1 ~ "Yes"), # Woman moved to a different region
      # Create a variable with the correct Adm1 region where applicable
      Adm1_moved = case_when(
        moved=="No" ~ Adm1,
        moved=="Yes" ~ Prev_Adm1,
        TRUE ~ NA
      ))
  
  # Get subset of data where moving is known
  data2 <- subset(data_merged, moved!="Unknown")
  
  # Number of observations where moving variables are available
  n2 <- dim(data2)[1]
  
  # If there is moving data, then continue
  if (dim(data2)[1]>0) {
    
    # Set different colnames for the drought data
    colnames(drought_dat) <- c("iso", "Adm1_moved", "year", "drought_prev")
    
    # Merge data
    data2 <- suppressMessages(left_join(data2, drought_dat))
    
    # Create variable showing misclassification of drought
    data2 <- data2 %>%
      mutate(drought_misclass = case_when(
        Adm1_moved=="Abroad" | Adm1_moved=="Other" ~ 1, # If they previously lived abroad, drought status is misclassified
        drought_prev==drought ~ 0,
        drought_prev!=drought ~ 1,
        TRUE ~ NA
      ))
    
    # Calculate proportion of observations misclassified
    prop <- sum(data2$drought_misclass)/n2
  } else {
    prop <- NA
  }
  
  # Return result
  country <- countrycode(iso, "iso3c", "un.name.en") # Country
  list(country, n, n2, round(prop, 3))
  
}

