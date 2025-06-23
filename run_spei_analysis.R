

# Set up the data ---------------------------------------------------------

# Get country codes
all_dat <- readRDS("data/all_dat.rds")
countries <- unique(all_dat$iso3)

# Dates to extract
st_dat <- "1901-01-16"
end_dat <- "2022-12-16"
dat_seq <- seq(as.Date(st_dat), as.Date(end_dat), by="months")

# Create SpatRaster
spei_rast <- terra::rast("data/spei12.nc")

# Iterate through all the countries
for (i in 1:length(countries)) {
  # Country code
  country_cde <- countries[i]
  print(paste0("Running code for ", country_cde, 
               " (", i, "/", length(countries), ")"))
  
  # Get regions
  regions <- gadm(country_cde, level=1, path="data", version="3.6")
  regions <- project(regions, "EPSG:4326")
  country <- regions$NAME_0[1]
  
  # Get average in each region
  # avg_spei <- zonal(spei_rast, regions, mean)
  avg_spei <- extract(spei_rast, regions, fun = mean, weights = TRUE, na.rm = TRUE)
  
  # Tidy up dataset
  avg_spei <- t(avg_spei)
  colnames(avg_spei) <- values(regions)$NAME_1
  avg_spei <- as.data.frame(avg_spei)
  avg_spei <- avg_spei[-1,]
  avg_spei$date <- dat_seq
  
  # Convert dataframe to long format
  avg_spei <- avg_spei %>%
    pivot_longer(cols = colnames(avg_spei)[1:(ncol(avg_spei)-1)], 
                 names_to = "adm1", values_to = "spei") %>%
    filter(complete.cases(.)) %>%
    mutate(adm0 = country,
           iso3 = country_cde,
           spei15 = ifelse(spei<(-1.5), 1, 0),
           spei20 = ifelse(spei<(-2), 1, 0)) %>%
    select(date, iso3, adm0, adm1, spei, spei15, spei20)
  
  avg_spei_dat <- as.data.frame(avg_spei)
  
  # Save dataframe
  saveRDS(avg_spei_dat, file = paste0("data/spei_data/spei_", country_cde, ".rds"))
}

# Put all files in a list
files <- list.files("data/spei_data/")
dat_list <- list()
for (i in 1:length(files)) {
  dat_list[[i]] <- readRDS(paste0("data/spei_data/", files[i]))
}

# Merge all spei datasets
dat_all_spei <- bind_rows(dat_list)

# Add year variable
dat_all_spei$year <- year(as.Date(dat_all_spei$date))

# Summarise drought variable by region-year
result <- dat_all_spei %>%
  group_by(iso3, adm1, year) %>%
  summarise(spei_15 = as.integer(any(spei15 == 1)),
            spei_20 = as.integer(any(spei20 == 1)))
colnames(result) <- c("iso", "Adm1", "year", "spei_15", "spei_20")
result$Adm1 <- paste(result$iso, result$Adm1, sep = ": ")

# Assign cohorts to the drought data based on treatment history
drought_cohort <- result %>%
  group_by(Adm1) %>%
  summarize(drought_sequence_15 = paste(spei_15, collapse = ""),
            drought_sequence_20 = paste(spei_20, collapse = ""))
result <- left_join(result, drought_cohort)

# Merge SPEI data with all_dat
all_dat2 <- left_join(all_dat, result)


# Run the analysis --------------------------------------------------------

# Data frames to store the results
results_main_15 <- data.frame()

# Countries run
countries_run <- countries[!countries %in% c("IND", "JAM", "TLS", "CRI")]

for (i in countries_run) {
  
  # Get data for a country
  dat <- subset(all_dat2, iso3==i)
  
  # De-mean age & rural status
  dat <- dat %>%
    group_by(Adm1, year) %>% # De-mean within each Adm1 and year
    mutate(age_dm = age_turned - weighted.mean(age_turned, Denorm_Wt),
           rural_dm = rural - weighted.mean(rural, Denorm_Wt)) %>%
    ungroup()
  
  # Add drought cohort
  dat$cohort_15 <- as.numeric(factor(dat$drought_sequence_15))
  dat$cohort_20 <- as.numeric(factor(dat$drought_sequence_20))
  
  # Run extended twfe model
  start_yr <- min(dat$year)
  mod <- feols(married ~ spei_15:i(cohort_15, i.year, ref=1, ref2=start_yr)/
                 (age_dm + rural_dm)|
                 cohort_15[age_turned, rural] + year[age_turned, rural],
               data=dat, vcov=~Adm1, weights=~Denorm_Wt,
               mem.clean=TRUE, notes = FALSE)
  
  # Calculate & save results
  res_main <- data.frame(aggregate(mod, c("^(?!.*:(age_dm|rural_dm)$).*")))
  res_main$iso <- i
  results_main_15 <- rbind(res_main, results_main_15)
}

# Store the results
colnames(results_main_15) <- c("estimate", "std.error", "statistic", "p.value", "iso")
results_main_15$conf.low <- results_main_15$estimate + qnorm(0.025)*results_main_15$std.error
results_main_15$conf.high <- results_main_15$estimate + qnorm(0.975)*results_main_15$std.error
write_xlsx(results_main_15, "results/etwfe_spei_15.xlsx")

# Combine with main results
results_main <- read_excel("results/etwfe_main.xlsx")
results_main_15 <- read_excel("results/etwfe_spei_15.xlsx")
results_main$type <- "EM-DAT"
results_main_15$type <- "SPEI_15"
results <- rbind(results_main, results_main_15)
results$country <- countrycode(results$iso, "iso3c", "country.name")

# Plot the results
p <- ggplot(results, aes(x = estimate, y = type)) +
  geom_point(size=1.5, shape=18) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0, linewidth = 0.5) +
  geom_vline(xintercept = 0, linewidth = 0.25, linetype="dotted") +
  labs(x = "Change in the prob. of marriage (95% CI)",
       y = "Exposure") + 
  theme_minimal() + # Starts with a minimal theme
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(),
    axis.ticks.length = unit(3, "pt"),
    axis.ticks = element_line(color = "black", linewidth = 0.25),
    plot.title = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  ) +
  xlim(-0.05, 0.05) +
  facet_wrap(~country, scales = "fixed", ncol=7,
             labeller = labeller(country = label_wrap_gen(width = 15)))

# Save the figure
ggsave(filename = "figures/main_spei.jpeg", plot = p, width = 8, height = 12, dpi = 300)

