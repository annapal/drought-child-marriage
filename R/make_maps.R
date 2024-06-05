
# Annual probability of marriage plot -------------------------------------

# Load the DHS/MICS data
load("data/data_merged_drought.Rdata")

all_data <- data.frame()

for (iso in names(data_merged_drought)) {
  
  data <- data_merged_drought[[iso]]
  
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
  
# Get list of total iso codes
all_countries <- country_codes()$ISO3

# Remove countries that can't be plotted (for some reason)
all_countries <- all_countries[!(all_countries%in%
                                   c("AIA","ATA","ABW","BVT","IOT","XCA",
                                     "CXR","XCL","CCK","COK","CUW","FLK",
                                     "GIB","HMD","KIR","MDV","MHL","MCO",
                                     "NIU","NFK","XPI","PCN","BLM","MAF",
                                     "SXM","SGS","XSP","VAT"))]

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
ggsave("figures/prob_map.jpeg", plot, width = 9, height = 4, dpi= 600)

# Location of drought plot ------------------------------------------------



