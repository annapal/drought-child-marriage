
# Annual probability of marriage plot -------------------------------------

plot_prob_map <- function(data_merged_drought) {

  all_data <- data.frame() # data frame to store probabilities for each region
  prop_country <- data.frame() # Dataframe to store probs for each country
  
  for (iso in names(data_merged_drought)) {
    
    data <- data_merged_drought[[iso]] # Get data for country
    
    # Calculate annual probability of marriage for each region
    prop <- data %>%
      group_by(Adm1) %>%
      summarise(prop = weighted.mean(married, Denorm_Wt))
    prop$GID_0 <- iso
    
    # Calculate annual probability of marriage in each country
    prop2 <- data %>%
      group_by(iso) %>%
      summarise(prop = weighted.mean(married, Denorm_Wt))
    
    # Merge data
    all_data <- rbind(all_data, prop)
    
    # Save country prevalence for descriptive purposes
    prop_country <- rbind(prop_country, prop2)
  }
    
  # Get list of iso codes for all countries
  all_countries <- setdiff(country_codes()$ISO3, c("AIA","ATA","ABW","BVT","IOT","XCA",
                                                   "CXR","XCL","CCK","COK","CUW","FLK",
                                                   "GIB","HMD","KIR","MDV","MHL","MCO",
                                                   "NIU","NFK","XPI","PCN","BLM","MAF",
                                                   "SXM","SGS","XSP","VAT"))
  
  # Get all country (Adm0) geometries
  countries <- gadm(all_countries, level=0, path="data", version="3.6")
  country_data <- st_as_sf(countries) # Set as sf object
  
  # Get all Adm1 geometries
  regions <- gadm(unique(all_data$GID_0), level=1, path="data", version="3.6")
  reg_data <- st_as_sf(regions) # Set as sf object
  
  # Merge region data with probability data
  plot_data <- left_join(reg_data, all_data, by=join_by(NAME_1==Adm1, GID_0==GID_0))
  
  # Plot the annual probabilities in each region
  plot <- 
    ggplot(country_data) +
    geom_sf(aes(color="No Data", geometry = geometry), lwd = 0) +
    scale_color_manual(values = c("No Data" = "black")) +
    geom_sf(data = plot_data, aes(fill = prop, geometry = geometry), lwd = 0) + # Region fills
    scale_fill_gradient(low = "beige", high = "darkred", na.value = "grey80") +
    ggtitle(~bold("a.")) +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.ticks = element_blank(),
          axis.text = element_blank(), legend.title=element_blank(),
          panel.background = element_blank()) +
    geom_sf(fill = NA, color = "black", lwd = 0.1) # Add country borders
  
  # Save the plot
  ggsave("figures/prob_map.jpeg", plot, width = 9, height = 4, dpi= 600)
  print("Map of annual probabilites saved in: figures/prob_map.jpeg")
  
  # Save country-level probabilites in excel
  write_xlsx(prop_country, "results/annual_prob_country.xlsx")
  print("Annual probabilities by country saved in: results/annual_prob_country.xlsx")
}

# Location of droughts plot ------------------------------------------------

plot_drought_map <- function(drought_panel_dat) {
  
  # Combine all drought data together across countries
  all_droughts <- do.call(rbind, drought_panel_dat) %>%
    filter(!is.na(event_no)) %>%
    select(-year, -drought) %>%
    distinct()
  
  # Get polygons
  regions <- gadm(unique(all_droughts$iso), level=1, path="data", version="3.6")
  reg_data <- st_as_sf(regions) # Set as sf object
  
  # Merge polygons to data
  all_droughts <- left_join(all_droughts, reg_data, by=join_by(Adm1==NAME_1, iso==GID_0))
  all_droughts$event_no2 <- paste(all_droughts$iso, all_droughts$event_no, sep = "-")
  
  # Merge polygons for each drought event into one
  all_droughts_merged <- all_droughts %>%
    group_by(event_no2) %>%
    summarise(geometry = st_union(geometry))
  
  # Get the centroid of each merged polygon
  all_droughts_merged <- all_droughts_merged %>%
    mutate(centroid = st_centroid(geometry))
  
  # Get the year of each drought
  all_droughts_merged$year <- as.numeric(substr(all_droughts_merged$event_no2, 5, 8))
  # Add decade variable
  all_droughts_merged$decade <- cut(all_droughts_merged$year, breaks = c(1979, 1990, 2000, 2010, 2020),
                            labels = c("1980s", "1990s", "2000s", "2010s"),
                            include.lowest = TRUE)
  # Add country variable
  all_droughts_merged$iso <- substr(all_droughts_merged$event_no2, 1, 3)
  
  
  # Plot drought locations
  plot2 <- 
    ggplot(country_data) +
    geom_sf(fill = "white", color = "black", lwd = 0.1) +
    geom_sf(data=all_droughts_merged, aes(color=decade, geometry=centroid),
               size=0.85) + 
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.ticks = element_blank(),
          axis.text = element_blank(), legend.title=element_blank(),
          panel.background = element_blank()) +
    xlab("") +
    ylab("") +
    scale_color_manual(values= c("#87CEFA", "#1E90FF", "#0000CD", "#191970")) +
    ggtitle(~bold("b.")) +
    guides(color=guide_legend(override.aes=list(fill=NA)))
  
  # If the figures folder doesn't exist, create it
  if (!dir.exists("figures")) {
    dir.create(dir_path)
  }
  
  # Save plot
  ggsave(paste0("figures/drought_loc_map.jpeg"), plot2, width = 9, height = 4, dpi= 600)
  print("Map of droughts saved in: figures/drought_loc_map.jpeg")
  
}