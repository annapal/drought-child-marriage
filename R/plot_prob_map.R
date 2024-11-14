
# Map showing the average annual probability of marriage in each region

plot_prob_map <- function(prop_region) {
  
  # Get list of iso codes for all countries
  all_countries <- country_codes()$ISO3
  
  # Get all country (Adm0) geometries
  countries <- gadm(all_countries, level=0, path="data", version="3.6")
  country_data <- st_as_sf(countries) # Set as sf object
  
  # Get all Adm1 geometries
  regions <- gadm(unique(prop_region$iso3), level=1, path="data", version="3.6")
  reg_data <- st_as_sf(regions) # Set as sf object
  
  # Merge region data with probability data
  plot_data <- left_join(reg_data, prop_region, by=join_by(NAME_1==Adm1, GID_0==iso3))
  
  # Plot the annual probabilities in each region
  plot <- 
    ggplot(country_data) +
    geom_sf(aes(color="Excluded from study", geometry = geometry), lwd = 0) +
    scale_color_manual(values = c("Excluded from study" = "black")) +
    geom_sf(data = plot_data, aes(fill = prob, geometry = geometry), lwd = 0) + # Region fills
    scale_fill_gradient(low = "#FAF3FC", high = "#2E0854", na.value = "grey80") +
    ggtitle(~bold("a.")) +
    labs(fill = "Average annual probability", color = "") +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.background = element_blank()) +
    geom_sf(fill = NA, color = "black", lwd = 0.1) + # Add country borders
    coord_sf(xlim = c(-85, 140), ylim = c(-40, 50))

  # Save the plot
  ggsave("figures/prob_map.jpeg", plot, width = 9, height = 4, dpi= 600)
  
}
