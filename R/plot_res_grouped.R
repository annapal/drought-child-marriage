
gdp_dat <- read_xlsx("data/gdp_2018.xlsx") %>%
  select(`Country Code`, `2018 [YR2018]`)
colnames(gdp_dat) <- c("iso", "gdp")
gdp_dat$gdp <- as.numeric(gdp_dat$gdp)

ag_dat <- read_xlsx("data/employment_in_agri_2018.xlsx") %>%
  select(`Country Code`, `2018 [YR2018]`)
colnames(ag_dat) <- c("iso", "ag")
ag_dat$ag <- as.numeric(ag_dat$ag)

results_plot <- results_all %>%
  left_join(gdp_dat) %>%
  left_join(ag_dat)

ggplot(results_plot, aes(x = ag, y = estimate)) +
  geom_point(size = 3) +  # Add points
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  # Add error bars
  geom_text_repel(aes(label = iso), box.padding = 0.3, max.overlaps = 10) +  # Add non-overlapping labels
  labs(x = "GDP", y = "Estimate", title = "GDP vs Estimate with Confidence Intervals") +
  theme_minimal()

# Wealth ------------------------------------------------------------------

countries <- unique(all_dat$iso)
regions_all <- data.frame()

for (iso in countries) {

  file <- paste0("data/relative-wealth-index-april-2021/", iso, "_relative_wealth_index.csv")

  if (file.exists(file)) {

    wealth_dat <- read_csv(file)
    points <- vect(wealth_dat, geom = c("longitude", "latitude"), crs = "EPSG:4326")
    template <- rast(ext = ext(points), resolution = 0.01, crs = "EPSG:4326")
    raster_obj <- rasterize(points, template, field = "rwi", fun = "mean")
    
    pop_raster <- population(2015, res=0.5, path="data")
    pop_raster_resampled <- resample(pop_raster, raster_obj, method = "bilinear")
    
    pop_weighted_rwi <- raster_obj * pop_raster_resampled

    regions <- gadm(iso, path="data", version="3.6")
    
    weighted_rwi_sum <- extract(pop_weighted_rwi, regions, fun = sum, na.rm = TRUE)
    pop_sum <- extract(pop_raster_resampled, regions, fun = sum, na.rm = TRUE)
    
    regions$weighted_avg_rwi <- weighted_rwi_sum[,2] / pop_sum[,2]
    regions_df <- as.data.frame(regions[, c("GID_0", "NAME_1", "GID_1", "weighted_avg_rwi")])

    regions_all <- rbind(regions_df, regions_all)
  }
}
regions_all <- regions_all %>% filter(weighted_avg_rwi!="NaN" | !is.na(weighted_avg_rwi))
saveRDS(regions_all, "data/wealth_data.rds")

regions_all <- readRDS("data/wealth_data.rds")
drought_dat <- readRDS("data/drought_panel_dat.rds")
drought_all <- do.call(rbind, drought_dat)

all_gadm <- st_as_sf(gadm(unique(drought_all$iso), path="data", version="3.6")) %>%
  select(GID_0, GID_1, NAME_1) %>%
  st_drop_geometry()
drought_all <- left_join(drought_all, all_gadm)
drought_all <- left_join(drought_all, regions_all)

result <- drought_all %>%
  filter(drought2 == 1) %>%  # Filter rows where drought2 == 1
  group_by(iso) %>%          # Group by iso
  summarise(
    avg_RWI = mean(weighted_avg_rwi, na.rm = TRUE)
  )

results_all <- read_xlsx("results/etwfe_main.xlsx")
results_merged <- left_join(results_all, result)

ggplot(results_merged, aes(x = avg_RWI, y = estimate, label = iso)) +
  geom_point() +              # Add points
  geom_text(nudge_y = 0.001) +  # Add labels with a small vertical adjustment
  labs(
    x = "RWI (Average)",
    y = "Estimate",
    title = "Scatter Plot of RWI vs Estimate"
  ) +
  theme_minimal()   

# Cropland area -----------------------------------------------------------

cropland_dat <- rast("data/Global_cropland_3km_2015.tif")

drought_dat <- readRDS("data/drought_panel_dat.rds")
drought_all <- do.call(rbind, drought_dat)
all_gadm <- gadm(unique(drought_all$iso), path="data", version="3.6")
all_gadm <- project(all_gadm, crs(cropland_dat))

# Step 1: Mask cropland raster by each polygon
# Create an empty list to store the results
cropland_proportions <- list()

# Loop over each polygon in the `all_gadm` SpatVector
for (i in 1:nrow(all_gadm)) {
  
  # Mask the raster to the polygon's extent
  mask_raster <- mask(cropland_dat, all_gadm[i, ])
  
  # Step 2: Calculate the number of cropland grid cells within the polygon
  # We'll assume that cropland area is represented by cells with a value of 1 (or any non-zero value)
  cropland_cells <- sum(values(mask_raster) == 1, na.rm = TRUE)  # Count cropland cells
  
  # Step 3: Calculate the total number of grid cells in the polygon
  total_cells <- sum(!is.na(values(mask_raster)))  # Count non-NA cells
  
  # Step 4: Calculate the proportion of cropland cells
  cropland_proportion <- cropland_cells / total_cells
  
  # Store the proportion in the list
  cropland_proportions[[i]] <- cropland_proportion
  
  print(i)
}

# Combine the results into a single vector or a data frame
cropland_proportions_df <- data.frame(GID_1 = all_gadm$GID_1, cropland_proportion = unlist(cropland_proportions))
saveRDS(cropland_proportions_df, "data/cropland_prop.rds")

all_gadm <- st_as_sf(gadm(unique(drought_all$iso), path="data", version="3.6")) %>%
  select(GID_0, GID_1, NAME_1) %>%
  st_drop_geometry()
colnames(all_gadm) <- c("iso", "GID_1", "Adm1")
drought_all <- left_join(drought_all, all_gadm)

drought_all <- left_join(drought_all, cropland_proportions_df)

result <- drought_all %>%
  filter(drought2 == 1) %>%  # Filter rows where drought2 == 1
  group_by(iso) %>%          # Group by iso
  summarise(
    avg_cropland = mean(cropland_proportion, na.rm = TRUE)
  )

results_all <- read_xlsx("results/etwfe_main.xlsx")
results_merged <- left_join(results_all, result)

ggplot(results_merged, aes(x = avg_cropland, y = estimate, label = iso)) +
  geom_point() +              # Add points
  geom_text(nudge_y = 0.001) +  # Add labels with a small vertical adjustment
  labs(
    x = "RWI (Average)",
    y = "Estimate",
    title = "Scatter Plot of RWI vs Estimate"
  ) +
  theme_minimal() 


