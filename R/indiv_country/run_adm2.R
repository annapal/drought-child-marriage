
# Function that runs Adm2 analysis for a particular country

run_adm2 <- function(drought_dat_adm2, data) {
  
  # Assign year after drought ends as 1
  drought_dat_adm2 <- drought_dat_adm2 %>%
    arrange(Reg, year) %>%
    group_by(Reg) %>%
    mutate(
      drought2=ifelse(lag(drought, default=0)==1, 1, drought),
      lagged=ifelse((drought2-drought==1), 1, 0) # Indicator where drought exposure is lagged
    ) %>%
    ungroup()
  
  # Assign cohorts based on treatment history
  drought_cohort <- drought_dat_adm2 %>%
    group_by(Reg) %>%
    summarize(drought_sequence = paste(drought2, collapse = ""))
  drought_cohort$cohort <- as.numeric(factor(drought_cohort$drought_sequence))
  drought_dat_adm2 <- suppressMessages(left_join(drought_dat_adm2, drought_cohort))
  
  # Merge drought data to marriage data & remove non-relevant years
  data <- data %>% select(-Reg) # Remove variable Reg, so they can be joined properly
  data <- suppressMessages(left_join(data, drought_dat_adm2) %>% filter(!is.na(cohort)))
  
  # De-mean age and rural-urban status
  data$rural <- ifelse(data$res=="rural", 1, 0) # Make indicator for rural
  data<- data %>%
    group_by(cohort, year) %>% # De-mean within each cohort and year
    mutate(age_dm = age_turned - mean(age_turned),
           rural_dm = rural - mean(rural)) %>%
    ungroup()
  
  # Run extended twfe model
  start_yr <- min(drought_dat_adm2$year)
  mod <- feols(married ~ drought2:i(cohort, i.year, ref=1, ref2=start_yr)/
                 (age_dm + rural_dm)|
                 cohort[age_turned, rural] + year[age_turned, rural], 
               data=data, vcov=~Adm2, weights=~Denorm_Wt,
               notes = FALSE, mem.clean = TRUE)
  
  # Calculate main TE
  result <- slopes(
    mod,
    newdata = subset(data, drought2==1), # Only region-years exposed to drought
    variables = "drought2",
    by = "drought2",
    wts = "Denorm_Wt"
  )
  
  # Return result
  result$iso <- iso
  result
}
