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

# Create data frames to store results -------------------------------------

results_all <- data.frame() # Main results

# Run the analysis -------------------------------------------------------

for (iso in unique(drought_dat_all$ISO)) {

  # Create the drought panel data
  drought_dat <- create_drought_panel(iso, drought_dat_all, gdis_all)
  
  # Read in the DHS-MICS data
  data <- readRDS(paste0("data/dhs-mics/", iso, ".rds"))
  
  # Merge DHS-MICS data with drought data
  data_merged <- suppressMessages(left_join(data, drought_dat) %>% filter(!is.na(cohort)))
  data_merged$rural <- ifelse(data_merged$res=="rural", 1, 0) # Make indicator for rural status
  
  # Run the model for the counterfactual
  first_stage <- feols(married ~ 1 |
                 cohort[age_turned, rural] + year[age_turned, rural],
               data=subset(data_merged, drought2==0), 
               vcov=~Adm1, weights=~Denorm_Wt)
  
  # Calculate the residuals
  data_merged$married_resid <- 
    data_merged$married - predict(first_stage, newdata = data_merged)
  
  # Calculate point estimate
  dat2 <- subset(data_merged, drought2==1)
  point_est <- weighted.mean(dat2$married_resid, dat2$Denorm_Wt)
  
  # Calculate the SE
  second_stage <- feols(
    married_resid ~ i(drought2, ref = 0),
    data = data_merged, 
    vcov=~Adm1, weights=~Denorm_Wt
  )
  
  # Save the results
  results <- broom::tidy(second_stage, conf.int = TRUE)[2,]
  results$iso <- iso
  results_all <- rbind(results_all, results)
  
  # Create event study plot
  event_study <- feols(
    married_resid ~ i(drought_time, ref = -1)|
      cohort[age_turned, rural] + year[age_turned, rural],
    data=subset(data_merged, drought2==0),
    # data=data_merged,
    vcov=~Adm1, weights=~Denorm_Wt
  )
  
  png(paste0("run2/event_study_plots/", iso, ".png"), width = 800, height = 600)
  coefplot(event_study)
  dev.off()

}

write_xlsx(results_all, "run2/all_tes.xlsx")

# Plot the main results
plot_main(results_all, path = "run2/tes_main.jpeg")
