
# Run the ETWFE model

# Load the DHS/MICS data and drought panel data
load("data/clean_data_long.RData")
load("data/emdat/drought_panel.Rdata")

# Create list to store models
all_models <- list()

# Create a list to store the DHS/MICS and drought merged data
data_merged_drought <- list()

# Iso codes of countries to include in the analysis
iso_cdes <- names(drought_panel_dat)

for (iso in iso_cdes) {
  
  # Get data for specific country
  data <- long_data[[iso]] %>% filter(!is.na(Adm1))
  drought_dat <- drought_panel_dat[[iso]]
  
  # Assign year after drought ends as 1
  drought_dat <- drought_dat %>%
    arrange(Adm1, year) %>%
    group_by(Adm1) %>%
    mutate(
      drought2=ifelse(lag(drought, default=0)==1, 1, drought),
      lagged=ifelse((drought2-drought==1), 1, 0), # Indicator where drought exposure is lagged
      event_no2 = ifelse(lagged==1, lag(event_no), event_no) # Add the event no. where lagged
    ) %>%
    ungroup()
  
  # Assign cohorts to the drought data based on treatment history
  drought_cohort <- drought_dat %>%
    group_by(Adm1) %>%
    summarize(drought_sequence = paste(drought2, collapse = ""))
  drought_cohort$cohort <- as.numeric(factor(drought_cohort$drought_sequence))
  drought_dat <- left_join(drought_dat, drought_cohort)
  
  # Create variable for years of consecutive exposure to droughts
  drought_dat$drought_yr <- 0
  for (i in 2:nrow(drought_dat)) {
    if (drought_dat$drought2[i]==1 & drought_dat$drought2[i-1]==0) {
      drought_dat$drought_yr[i] <- 1 # First year of drought
    } else if (drought_dat$drought2[i]==1 & drought_dat$drought2[i-1]==1) {
      drought_dat$drought_yr[i] <- drought_dat$drought_yr[i-1] + 1 # Subsequent years
    }
  }
  
  # Create a variable for exposure to 3 years of drought
  # (i.e. all regions and years where there was at least 3 years of exposure)
  drought_dat$drought_3yr <- 0
  for (i in 3:nrow(drought_dat)) {
    if (drought_dat$drought_yr[i]==3) {
      drought_dat$drought_3yr[(i-2):i] <- 1
    }
  }
  
  # Merge drought data to marriage data & remove non-relevant years
  data <- left_join(data, drought_dat) %>% filter(!is.na(cohort))
  
  # De-mean age and rural-urban status
  data$rural <- ifelse(data$res=="rural", 1, 0) # Make indicator for rural
  data<- data %>%
    group_by(cohort, year) %>% # De-mean within each cohort and year
    mutate(age_dm = age_turned - mean(age_turned),
           rural_dm = rural - mean(rural)) %>%
    ungroup()

  # Run extended twfe model
  start_yr <- min(drought_dat$year)
  mod <- feols(married ~ drought2:i(cohort, i.year, ref=1, ref2=start_yr)/
                 (age_dm + rural_dm)|
                 cohort[age_turned, rural] + year[age_turned, rural],
               data=data, vcov=~Adm1, weights=~Denorm_Wt,
               mem.clean=TRUE)

  # Save model to list
  all_models[[iso]] <- mod
  
  # Save the merged data to list
  data_merged_drought[[iso]] <- data
  
} 

# save(all_models, file="data/all_models.Rdata")
save(data_merged_drought, file="data/data_merged_drought.Rdata")

