## Load packages
source("./packages.R")

## Load R files
lapply(list.files("./R", full.names = TRUE, recursive = TRUE), source)

# Load the data --------------------------------------------------------

# EM-DAT drought data
drought_dat_all <- suppressWarnings(read_excel("data/emdat/emdat_drought_events_updated.xlsx")) %>%
  filter(Include=="Yes")

# GDIS data 
gdis_all <- suppressWarnings(read_excel("data/emdat/pend-gdis-1960-2018-disasterlocations_updated.xlsx"))

# Prepare the data --------------------------------------------------------

all_dat <- data.frame()

for (iso in unique(drought_dat_all$ISO)) {
  
  # Create the drought panel data
  drought_dat <- create_drought_panel(iso, drought_dat_all, gdis_all)
  
  # Read in the DHS-MICS data
  data <- readRDS(paste0("data/dhs-mics/", iso, ".rds"))
  
  # Merge DHS-MICS data with drought data
  data_merged <- suppressMessages(left_join(data, drought_dat) %>% filter(!is.na(cohort)))
  data_merged$rural <- ifelse(data_merged$res=="rural", 1, 0) # Make indicator for rural status
  
  # Make cohorts & Adm1 unique to country
  data_merged$cohort <- paste(iso, data_merged$cohort, sep=": ")
  data_merged$Adm1 <- paste(iso, data_merged$Adm1, sep=": ")
  
  # Make year variable that is unique to country
  data_merged$year_iso <- paste(data_merged$year, iso, sep = "_")
  
  # Add to dataframe
  all_dat <- rbind(all_dat, data_merged)
}

saveRDS(all_dat, file="data/all_dat.rds")

# Run the analysis -------------------------------------------------------

# Run the model for the counterfactual
# first_stage <- feols(married ~ 1 |
#                        cohort[age_turned, rural] + year_iso[age_turned, rural],
#                      data=subset(all_dat, drought2==0),
#                      vcov=~Adm1, weights=~Denorm_Wt)

first_stage <- feols(married ~ 1 |
                       cohort[age_turned, rural] + year[age_turned, rural] +
                       year_iso[age_turned, rural],
                     data=subset(all_dat, drought2==0),
                     vcov=~Adm1, weights=~Denorm_Wt)

# Calculate the residuals
all_dat$married_resid <- 
  all_dat$married - predict(first_stage, newdata = all_dat)

results_all <- data.frame() # Main results

for (i in unique(drought_dat_all$ISO)) {

  # Calculate the SE
  second_stage <- feols(
    married_resid ~ i(drought2, ref = 0),
    data = subset(all_dat, iso3==i),
    vcov=~Adm1, weights=~Denorm_Wt
  )
  
  # Save the results
  results <- broom::tidy(second_stage, conf.int = TRUE)[2,]
  results$iso <- i
  results_all <- rbind(results_all, results)
}

write_xlsx(results_all, "run3/all_tes.xlsx")

# Plot the results --------------------------------------------------------

results_all2 <- read_xlsx("run2/all_tes.xlsx")

# Add an indicator column to each dataset
results_all$dataset <- "combined data"
results_all2$dataset <- "sep data"

# Combine the datasets
combined_results <- bind_rows(results_all, results_all2)

# Plotting
ggplot(combined_results, aes(x = iso, y = estimate, color = dataset)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), 
                  position = position_dodge(width = 0.5), # Side-by-side positioning
                  size = 0.4, shape=16, stroke=0.2) +
  labs(x = "Country", y = "Estimate", title = "Comparison of Estimates and Confidence Intervals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_manual(values = c("blue", "red")) +
  coord_flip()
ggsave("run3/coefs.jpeg")
  
