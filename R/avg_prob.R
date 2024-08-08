
# Calculate the average annual probability of marriage

avg_prob_country <- function(data_merged) {
  
  prop2 <- data_merged %>%
    group_by(iso3) %>%
    summarise(prop = weighted.mean(married, Denorm_Wt))
  
  prop2
}

avg_prob_region <- function(data_merged) {
  
  prop <- data_merged %>%
    group_by(Adm1) %>%
    summarise(prop = weighted.mean(married, Denorm_Wt))
  prop$GID_0 <- iso
  
  prop
}