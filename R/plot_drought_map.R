# Map showing location of drought events

plot_drought_map <- function() {
  
  drought_panel_dat <- readRDS("data/drought_panel_dat.rds")
  
  # Combine all drought data together across countries
  all_droughts <- do.call(rbind, drought_panel_dat)
  
  # Get list of iso codes for all countries
  all_countries <- country_codes()$ISO3
  
  # Get all country (Adm0) geometries
  countries <- gadm(all_countries, level=0, path="data", version="3.6")
  country_data <- st_as_sf(countries) # Set as sf object
  
  # Create indicator for whether country is included in the analysis
  country_data$included <- ifelse(country_data$GID_0 %in% names(drought_panel_dat), 1, 0)
  
  # Get polygons
  regions <- gadm(unique(all_droughts$iso), level=1, path="data", version="3.6")
  reg_data <- st_as_sf(regions) # Set as sf object
  
  # Count number of droughts
  result <- all_droughts %>%
    group_by(iso, Adm1) %>%
    summarize(unique_event_count = n_distinct(event_no, na.rm=TRUE)) %>%
    ungroup()
  
  # Merge polygons to data
  result <- left_join(result, reg_data, by=join_by(Adm1==NAME_1, iso==GID_0))
  
  # Plot drought locations
  plot2 <- 
    ggplot(country_data) +
    geom_sf(aes(color="Excluded from study", geometry = geometry), lwd = 0) +
    scale_color_manual(values = c("Excluded from study" = "black")) +
    geom_sf(data = result, aes(fill = unique_event_count, geometry = geometry), lwd = 0) + # Region fills
    scale_fill_gradient(low = "#F4E7D4", high = "#B33A00", na.value = "grey80") +
    ggtitle(~bold("b.")) +
    labs(fill = "Number of droughts", color = "") +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.background = element_blank()) +
    geom_sf(fill = NA, color = "black", lwd = 0.1) + # Add country borders
    coord_sf(xlim = c(-85, 140), ylim = c(-40, 50))
  
  # Save plot
  ggsave(paste0("figures/drought_loc_map.jpeg"), plot2, width = 9, height = 4, dpi= 600)
}
