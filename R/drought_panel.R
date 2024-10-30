
# Create drought panel data and plot the panel for a specific country

create_drought_panel <- function(iso, drought_dat_all, gdis_all) {
  
  # Get drought events for a specific country
  drought_subs <- subset(drought_dat_all, ISO==iso)
  
  # Get min and max years of data for that country
  min_yr <- drought_subs$`Panel Start`[1]
  max_yr <- drought_subs$`Panel End`[1]
  
  # Get region names at the first administrative division
  reg_names <- gadm(iso, version="3.6", path="data")$NAME_1
  
  # Set up panel data frame
  panel_dat <- data.frame(iso=iso, Adm1=reg_names,
                          year=rep(min_yr:max_yr, each=length(reg_names)),
                          drought=0, event_no=NA)
  
  # Add data from each drought event
  for (i in 1:nrow(drought_subs)) {
    event_no <- substr(drought_subs$`Dis No`[i], 1, 9) # EMDAT disaster number
    event_start <- drought_subs$`Start Year`[i] # Year of event start
    event_end <- drought_subs$`End Year`[i] # Year of event end
    
    # Get GDIS information
    gdis <- gdis_all %>%
      filter(disasterno==event_no & iso3==iso)
    
    # Get regions that are exposed
    treated_gdis <- unique(gdis$adm1)
    
    # Fix names in GDIS that are slightly different from GADM by matching to closest string
    treated <- sapply(treated_gdis, function(x) {
      distances <- stringdist::stringdist(x, reg_names)
      closest_match <- reg_names[which.min(distances)]
      closest_match
    })
    
    # Set region-years to 1 where drought event was occurring and add event number
    panel_dat[(panel_dat$Adm1 %in% treated) & (panel_dat$year %in% event_start:event_end),
              "drought"] <- 1
    panel_dat[(panel_dat$Adm1 %in% treated) & (panel_dat$year %in% event_start:event_end),
              "event_no"] <- event_no
  }
  
  # Plot the panel for country
  panel_dat$drought_plot <- factor(panel_dat$drought, levels = c(0, 1), labels = c("No Drought", "Drought"))
  ggplot(panel_dat, aes(x = year, y = Adm1, fill = drought_plot)) +
    geom_tile(color = "white") +
    scale_fill_manual(values = c("lightblue", "darkblue")) +
    labs(x = "Year", y = "Adm1", title = paste0(iso, ": Droughts")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Save the panel plot
  ggsave(paste0(getwd(),"/data/emdat/panel_plots/", iso, "_panel.jpeg"),
         height = max(length(reg_names)/5, 5), width = 10)
  
  # Assign year after drought ends as 1
  panel_dat <- panel_dat %>%
    arrange(Adm1, year) %>%
    group_by(Adm1) %>%
    mutate(
      drought2=ifelse(lag(drought, default=0)==1, 1, drought), # New drought exposure variable
      lagged=ifelse((drought2-drought==1), 1, 0), # Indicator of when drought exposure is lagged
      event_no2 = ifelse(lagged==1, lag(event_no), event_no) # Event no. for new drought variable
    ) %>%
    ungroup()
  
  # Assign cohorts to the drought data based on treatment history
  drought_cohort <- panel_dat %>%
    group_by(Adm1) %>%
    summarize(drought_sequence = paste(drought2, collapse = ""))
  drought_cohort$cohort <- as.numeric(factor(drought_cohort$drought_sequence))
  panel_dat <- suppressMessages(left_join(panel_dat, drought_cohort))
  
  # Create variable showing consecutive years of exposure to drought
  panel_dat$drought_yr <- NA
  for (i in 2:nrow(panel_dat)) {
    if (panel_dat$drought2[i]==1 & panel_dat$drought2[i-1]==0) {
      panel_dat$drought_yr[i] <- 0 # First year of drought
    } else if (panel_dat$drought2[i]==1 & panel_dat$drought2[i-1]==1) {
      panel_dat$drought_yr[i] <- panel_dat$drought_yr[i-1] + 1 # Subsequent years of exposure
    }
  }
  
  # Create a variable for exposure to 3 years of drought
  # (i.e. all regions and years where there was at least 3 years of exposure)
  panel_dat$drought_3yr <- NA
  for (i in 3:nrow(panel_dat)) {
    if (!is.na(panel_dat$drought_yr[i]) & (panel_dat$drought_yr[i]==2)) {
      panel_dat$drought_3yr[(i-2):i] <- 1
    }
  }
  
  # Add time to drought variable
  panel_dat <- panel_dat %>%
    arrange(Adm1, year) %>%
    group_by(Adm1) %>%
    mutate(
      # Identify the first occurrence of drought (drought2 == 1) in each drought period
      drought_start = ifelse(drought2 == 1 & lag(drought2, default = 0) == 0, row_number(), NA),
      drought_start = zoo::na.locf(drought_start, na.rm = FALSE, fromLast = TRUE),  # Fill forward until the next drought reset
      drought_start = ifelse(is.na(drought_start), max(row_number(), na.rm = TRUE) + 1, drought_start),  # Handle trailing NAs
      
      # Calculate drought_time with a countdown capped at -5 before each drought
      drought_time = ifelse(
        drought2 == 1, 0,                                     # Set 0 during drought years
        pmax(-(drought_start - row_number()), -6)             # Cap countdown at -5 for years before drought
      )
    ) %>%
    ungroup() %>%
    select(-drought_start)
  panel_dat$drought_time <- ifelse(panel_dat$drought_time==-6, -1, panel_dat$drought_time)
  panel_dat$drought_time <- panel_dat$drought_time + ifelse(is.na(panel_dat$drought_yr), 0, panel_dat$drought_yr)
  
  # Return the panel data
  panel_dat
  
}