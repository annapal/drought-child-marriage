
# Annual probability of marriage plot -------------------------------------

# Load the DHS/MICS data
load("data/data_merged_drought.Rdata")

all_data <- data.frame() # data frame to store probabilities for each country

for (iso in names(data_merged_drought)) {
  
  data <- data_merged_drought[[iso]] # Get data for country
  
  # Calculate annual probability of marriage for each region
  prop <- data %>%
    group_by(Adm1) %>%
    summarise(prop = mean(married))
  
  # Get polygons
  regions <- gadm(iso, level=1, path="data", version="3.6")
  reg_data <- st_as_sf(regions) # Set as sf object
  
  # Merge data
  plot_data <- left_join(reg_data, prop, by=join_by(NAME_1==Adm1))
  all_data <- rbind(all_data, plot_data)
}
  
# Get list of iso codes for all countries
all_countries <- setdiff(country_codes()$ISO3, c("AIA","ATA","ABW","BVT","IOT","XCA",
                                                 "CXR","XCL","CCK","COK","CUW","FLK",
                                                 "GIB","HMD","KIR","MDV","MHL","MCO",
                                                 "NIU","NFK","XPI","PCN","BLM","MAF",
                                                 "SXM","SGS","XSP","VAT"))

# Get all country (Adm0) geometries
countries <- gadm(all_countries, level=0, path="data", version="3.6")
country_data <- st_as_sf(countries)

# Plot the annual probabilities
plot <- 
  ggplot(country_data) +
  geom_sf(data = all_data, aes(fill = prop, geometry = geometry), lwd = 0) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.1, 0.25), legend.title=element_blank(),
        panel.background = element_blank()) +
  scale_fill_gradient(low = "beige", high = "darkred") +
  geom_sf(fill = NA, color = "black", lwd = 0.1)

# Save the plot
ggsave("figures/prob_map.jpeg", plot, width = 9, height = 4, dpi= 600)

# Location of droughts plot ------------------------------------------------

# Load the drought data
load("data/emdat/drought_panel.Rdata")
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

# Get the centroid of each polygon
all_droughts_merged <- all_droughts_merged %>%
  mutate(centroid = st_centroid(geometry))

# Get the year of each drought
all_droughts_merged$year <- as.numeric(substr(all_droughts_merged$event_no2, 5, 8))
# Add decade variable
all_droughts_merged$decade <- cut(all_droughts_merged$year, breaks = c(1979, 1990, 2000, 2010, 2020),
                          labels = c("1980s", "1990s", "2000s", "2010s"),
                          include.lowest = TRUE)

# Plot drought locations
plot2 <- 
  ggplot(country_data) +
  geom_sf(fill = "white", color = "black", lwd = 0.1) +
  geom_sf(data=all_droughts_merged, aes(color=decade, geometry=centroid),
             size=0.85) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.1, 0.25), legend.title=element_blank(),
        panel.background = element_blank()) +
  xlab("") +
  ylab("") +
  scale_color_manual(values= c("#87CEFA", "#1E90FF", "#0000CD", "#191970")) +
  guides(color=guide_legend(override.aes=list(fill=NA)))

# Save plot
ggsave(paste0("figures/drought_loc_map.jpeg"), plot2, width = 9, height = 4, dpi= 600)

