
# Load the DHS/MICS data and drought panel data
load("data/data_merged_drought.Rdata")

# Calculate how many surveys have moving variables
all_surv <- data.frame()
for (iso in names(data_merged_drought)) {
  
  # Get country data
  data <- data_merged_drought[[iso]]
  
  data <- data %>%
    mutate(
      # Create variable indicating if woman has moved region
      moved = case_when(
        is.na(al_lived) ~ "Unknown", # Unknown if woman moved
        al_lived==1 ~ "No", # Woman always lived in same location
        al_lived==0 & age-yrs_lived<13 ~ "No", # Woman moved prior to being at risk of child marriage
        al_lived==0 & Adm1==Prev_Adm1 ~ "No", # Woman has moved, but moved within same region
        al_lived==0 & is.na(Prev_Adm1) ~ "Unknown", # Woman has moved, but to unknown region
        Adm1!=Prev_Adm1 ~ "Yes")) # Woman moved to a different region
  
  # Save surveys where previous location is known
  data2 <- subset(data, moved!="Unknown")
  if (dim(data2)[1]>0) {
    dat_country <- data.frame(iso = iso, surv = unique(data2$surveyid))
    all_surv <- rbind(all_surv, dat_country)
  }
}

