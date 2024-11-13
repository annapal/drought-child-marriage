
# Calculate the average annual probability of marriage

avg_prob_country <- function(all_dat) {
  
  # Calculate probability
  prob <- all_dat %>%
    group_by(iso3) %>%
    summarise(prob = weighted.mean(married, Denorm_Wt))
  
  # Save the results
  write_xlsx(prob, "results/prop_country.xlsx")
  
  # Return the results
  prob
}

avg_prob_region <- function(all_dat) {
  
  # Calculate probability
  prob <- all_dat %>%
    group_by(iso3, Adm1) %>%
    summarise(prob = weighted.mean(married, Denorm_Wt))
  
  # Remove iso from Adm1 name
  prob$Adm1 <- substring(prob$Adm1, 6)
  
  # Save the results
  write_xlsx(prob, "results/prop_region.xlsx")
  
  # Return the results
  prob
}

# Calculate the proportion of time exposed to droughts

prop_py_exposed <- function(all_dat) {
  
  # Calculate proportion
  prop <- all_dat %>%
    group_by(iso3) %>%
    summarise(prop = weighted.mean(drought2, Denorm_Wt))
  
  # Save the results
  write_xlsx(prop, "results/prop_drought.xlsx")
  
  # Return the results
  prop
}
