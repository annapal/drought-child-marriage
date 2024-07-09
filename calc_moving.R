
# Plot how often women move from one region to another

# Load the DHS/MICS data and drought panel data
load("data/data_merged_drought.Rdata")

# Dataframe to store data from all countries
all_dat <- data.frame()

for (iso in names(data_merged_drought)) {
  data <- data_merged_drought[[iso]]
  
  # Create variable indicating if woman has moved region
  data <- data %>%
    mutate(moved = case_when(
      is.na(al_lived) ~ "Unknown", # Unknown if woman moved
      al_lived==1 ~ "No", # Woman always lived in same location
      al_lived==0 & age-yrs_lived<13 ~ "No", # Woman moved prior to being at risk of child marriage
      al_lived==0 & Adm1==Prev_Adm1 ~ "No", # Woman has moved, but moved within same region
      al_lived==0 & is.na(Prev_Adm1) ~ "Unknown", # Woman has moved, but to unknown region
      Adm1!=Prev_Adm1 ~ "Yes" # Woman moved to a different region
    ))
  
  # Store data in dataframe
  dat <- list(iso, sum(data$moved=="Yes"), sum(data$moved=="No"), sum(data$moved=="Unknown"))
  all_dat <- rbind(all_dat, dat)
}

# Set colnames for the data
colnames(all_dat) <- c("iso", "moved_yes", "moved_no", "moved_unknown")

# Remove countries where no data on movement is available
all_dat <- all_dat %>%
  filter(moved_no!=0 | moved_yes!=0)

# Reshape the data from wide to long format
all_dat <- all_dat %>%
  pivot_longer(cols = starts_with("moved_"), names_to = "moved", values_to = "value") %>%
  filter(moved!="moved_unknown")

# Calculate proportions for each country
all_dat <- all_dat %>%
  group_by(iso) %>%
  mutate(proportion = value / sum(value)) %>%
  ungroup()

# Reverse the order of the categories
all_dat$moved <- factor(all_dat$moved, levels = c("moved_unknown", "moved_no", "moved_yes"))

# Add country names to the data
all_dat$country <- countrycode(all_dat$iso, "iso3c", "country.name")
all_dat$country <- factor(all_dat$country, levels = rev(unique(all_dat$country)))

# Create the bar plot showing proportions
ggplot(all_dat, aes(x = country, y = proportion, fill = moved)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("#5B7DB1", "#BC5B57"),
                    labels = c("No", "Yes")) +
  labs(
    x = "Country",
    y = "Percent of sample",
    fill = "Moved region"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(), 
  )
  
# Save barplot
ggsave("figures/prop_moved.jpeg", width = 6, height = 10, dpi= 600)

dat_moved <- all_dat %>%
  filter(moved=="moved_yes")

