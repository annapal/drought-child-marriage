
merge_drought_dat <- function(drought_dat, data) {
  
  # Merge drought data to marriage data & remove non-relevant years
  data <- suppressMessages(left_join(data, drought_dat) %>% filter(!is.na(cohort)))
  
  # De-mean age and rural-urban status
  data$rural <- ifelse(data$res=="rural", 1, 0) # Make indicator for rural status
  data<- data %>%
    group_by(cohort, year) %>% # De-mean within each cohort and year
    mutate(age_dm = age_turned - mean(age_turned),
           rural_dm = rural - mean(rural)) %>%
    ungroup()
  
  # Return the dataframe
  data
}
