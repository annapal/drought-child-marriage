
# Create drought panel data and plot the panel for a specific country at Adm2 level

create_drought_panel_adm2 <- function(iso, drought_dat_all, gdis_all_adm2) {
  
  drought_subs <- subset(drought_dat_all, ISO==iso) # Get subset of drought data
  
  # Get min and max years of data for country
  min_yr <- drought_subs$`Panel Start`[1]
  max_yr <- drought_subs$`Panel End`[1]
  
  # Get region names
  reg_names_2 <- gadm(iso, level=2, version="3.6", path="data")$NAME_2
  reg_names_1 <- gadm(iso, level=2, version="3.6", path="data")$NAME_1
  
  # Make dataframe
  panel_dat <- data.frame(iso=iso, Adm1= reg_names_1, Adm2=reg_names_2, 
                          Reg=paste(reg_names_1, reg_names_2, sep = " - "),
                          year=rep(min_yr:max_yr, each=length(reg_names_1)),
                          drought=0, event_no=NA)
  
  for (i in 1:nrow(drought_subs)) {
    event_no <- substr(drought_subs$`Dis No`[i], 1, 9) # EMDAT disaster number
    event_start <- drought_subs$`Start Year`[i] # Year of event start
    event_end <- drought_subs$`End Year`[i] # Year of event end
    
    # Get GDIS information
    gdis <- gdis_all_adm2 %>%
      filter(disasterno==event_no & iso3==iso)
    gdis$Reg <- paste(gdis$adm1, gdis$adm2, sep = " - ")
    
    # Get regions that are exposed
    treated <- unique(gdis$Reg)
    
    # Apply indicator for drought region
    panel_dat[(panel_dat$Reg %in% treated) & (panel_dat$year %in% event_start:event_end),
              "drought"] <- 1
    panel_dat[(panel_dat$Reg %in% treated) & (panel_dat$year %in% event_start:event_end),
              "event_no"] <- event_no
  }
  
  # Plot the panel
  panel_dat$drought_plot <- factor(panel_dat$drought, levels = c(0, 1), labels = c("No Drought", "Drought"))
  ggplot(panel_dat, aes(x = year, y = Reg, fill = drought_plot)) +
    geom_tile(color = "white") +
    scale_fill_manual(values = c("lightblue", "darkblue")) +
    labs(x = "Year", y = "Adm1-Adm2", title = paste0(iso, ": Droughts")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Save the panel plot
  ggsave(paste0(getwd(),"/data/emdat/panel_plots_adm2/", iso, "_panel.jpeg"),
         height = max(length(reg_names_2)/5, 5), width = 10)
  
  # Assign year after drought ends as 1
  panel_dat <- panel_dat %>%
    arrange(Reg, year) %>%
    group_by(Reg) %>%
    mutate(
      drought2=ifelse(lag(drought, default=0)==1, 1, drought),
      lagged=ifelse((drought2-drought==1), 1, 0) # Indicator where drought exposure is lagged
    ) %>%
    ungroup()
  
  # Assign cohorts based on treatment history
  drought_cohort <- panel_dat %>%
    group_by(Reg) %>%
    summarize(drought_sequence = paste(drought2, collapse = ""))
  drought_cohort$cohort <- as.numeric(factor(drought_cohort$drought_sequence))
  panel_dat <- suppressMessages(left_join(panel_dat, drought_cohort))
  
  # Return the panel data
  panel_dat
  
}
