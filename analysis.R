library(readxl)
library(writexl)
library(tidyverse)
library(broom)
library(fixest)

# Load in the data
all_dat <- readRDS("data/all_dat.rds") # contains data from all countries
countries <- unique(all_dat$iso3) # List of countries included in the analysis

# OPTION 1: Estimate baseline model in each country separately ------------

results_all <- data.frame() # Dataframe to store all results

for (i in countries) {
  
  # Get country-specific data
  data_merged <- subset(all_dat, iso3==i)
  
  # Run the model on untreated observations
  first_stage <- feols(married ~ 1 |
                         cohort[age_turned, rural] + year[age_turned, rural], # Age & rural specific FEs
                       data=subset(data_merged, drought2==0), # Use only untreated observations
                       vcov=~Adm1, # Cluster SEs by state/province
                       weights=~Denorm_Wt) # Weight using survey sample weights
  
  # Calculate the residuals
  data_merged$married_resid <- 
    data_merged$married - predict(first_stage, newdata = data_merged)
  
  # Calculate point estimate of the treatment effect
  dat2 <- subset(data_merged, drought2==1)
  point_est <- weighted.mean(dat2$married_resid, dat2$Denorm_Wt)
  
  # Calculate the SE of the treatment effect using the residuals
  second_stage <- feols(
    married_resid ~ i(drought2, ref = 0),
    data = data_merged, 
    vcov=~Adm1, weights=~Denorm_Wt
  )
  
  # Save the treatment effects in one dataframe
  results <- broom::tidy(second_stage, conf.int = TRUE)[2,]
  results$iso <- i
  results_all <- rbind(results_all, results)
}

write_xlsx(results_all, "all_tes_opt1.xlsx") # Save results

# OPTION 2: Estimate baseline model together ------------------------------

# Run the baseline model using untreated observations from all countries
first_stage <- feols(married ~ 1 |
                       cohort[age_turned, rural] + year[age_turned, rural] +
                       year_iso[age_turned, rural], # Include a specific time FE for each country
                     data=subset(all_dat, drought2==0),
                     vcov=~Adm1, weights=~Denorm_Wt)

# Calculate the residuals
all_dat$married_resid <- 
  all_dat$married - predict(first_stage, newdata = all_dat)

# Calculate the SE
second_stage <- feols(
  married_resid ~ i(drought2, iso3, ref = 0),
  data = all_dat,
  vcov=~Adm1, weights=~Denorm_Wt
)

# Save the results
results_all2 <- broom::tidy(second_stage, conf.int = TRUE)[2:62,]
results_all2$iso <- substr(results_all2$term, nchar(results_all2$term) - 2, nchar(results_all2$term)) # Extract country code
write_xlsx(results_all2, "all_tes_opt2.xlsx")

# Plot the estimates together ---------------------------------------------

# Add an indicator column to each dataset
results_all$dataset <- "option 1"
results_all2$dataset <- "option 2"

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
ggsave("coefs_opt1_opt2.jpeg")

