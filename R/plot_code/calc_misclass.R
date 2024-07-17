
# Estimate the proportion of observations that are misclassified based on available data

calc_misclassification <- function(data_merged_drought, drought_panel_dat) {

  # Dataframe to store all country data
  dat_all <- data.frame()
  
  for (iso in names(data_merged_drought)) {
    
    # Get data for country
    data <- data_merged_drought[[iso]]
    drought_dat <- drought_panel_dat[[iso]] %>%
      select(-event_no)
    
    # Sample size for each country
    n <- nrow(data)
    
    data <- data %>%
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
    data2 <- subset(data, moved!="Unknown")
    
    # Number of observations where moving variables are available
    n2 <- dim(data2)[1]
    
    # If there is moving data, then continue
    if (dim(data2)[1]>0) {
    
      # Set different colnames for the drought data
      colnames(drought_dat) <- c("iso", "Adm1_moved", "year", "drought_prev")
      
      # Merge data
      data2 <- left_join(data2, drought_dat)
      
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
    
    # Save result to dataframe
    country <- countrycode(iso, "iso3c", "un.name.en") # Country
    dat_all <- rbind(dat_all, list(country, n, n2, round(prop, 3)))
  }
  
  # If the results folder doesn't exist, create it
  if (!dir.exists("results")) {
    dir.create("results")
  }
  
  # Save table to spreadsheet
  colnames(dat_all) <- c("country", "n_total", "n_move", "prop")
  write_xlsx(dat_all, "results/misclassification.xlsx")
  print("Proportion misclassified saved in: results/misclassification.xlsx")
  
  # Make plot showing estimated proportion with misclassified exposure status
  dat_all2 <- subset(dat_all, !is.na(prop))
  dat_all2$country <- factor(dat_all2$country, levels = rev(unique(dat_all2$country)))
  ggplot(dat_all2, aes(x = country, y = prop)) +
    geom_bar(stat = "identity") +
    labs(
      x = "Country",
      y = "Proportion"
    ) +
    coord_flip() +
    ylim(0,1) + 
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(), 
    )
  
  # If the figures folder doesn't exist, create it
  if (!dir.exists("figures")) {
    dir.create("figures")
  }
  
  # Save plot
  ggsave("figures/prop_misclassified.jpeg", width = 6, height = 8, dpi= 600)
  print("Plot showing proportion misclassified saved in: figures/prop_misclassified.jpeg")
}
