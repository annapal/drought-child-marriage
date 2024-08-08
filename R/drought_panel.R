
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
  
  # Return the panel data
  panel_dat
  
}