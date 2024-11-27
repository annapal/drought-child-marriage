## Load packages
source("./packages.R")

# Load the merged data
all_dat <- readRDS("data/all_dat.rds")

# Plot years of data & timing of droughts ------------------------------------
summary_df <- all_dat %>%
  group_by(iso) %>%
  summarise(
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE)
  ) %>%
  mutate(
    id = row_number(),
    country = countrycode(iso, "iso3c", "country.name")
  )

drought_summary_df <- all_dat %>%
  group_by(iso, year) %>%
  summarize(
    drought = max(drought, na.rm = TRUE),
    event_no = first(event_no[!is.na(event_no)])) %>%
  filter(drought==1) %>%
  mutate(
    event_no = paste(iso, event_no, sep = "-")
  ) %>%
  distinct(event_no, .keep_all = TRUE) %>%
  ungroup()
drought_summary_df <- left_join(drought_summary_df, summary_df)

ggplot(summary_df, aes(y = country)) +
  geom_rect(aes(xmin = min_year, xmax = max_year, 
                ymin = id - 0.4, ymax = id + 0.4, 
                fill = "Years of Data"),  # Fill aesthetic for the legend
            alpha = 0.5) +
  geom_point(data = drought_summary_df[drought_summary_df$drought == 1, ],
             aes(x = year, y = id, color = "Drought Event"),  # Color aesthetic for the legend
             size = 2) + 
  scale_y_discrete(limits = summary_df$country) +
  labs(x = "Year", y = "Country", fill = NULL, color = NULL) +  # Set legend titles to NULL
  scale_fill_manual(values = "grey") +  # Set fill color for legend
  scale_color_manual(values = "black") +  # Set point color for legend
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
ggsave("figures2/datasets.jpeg", height = 8, width = 6)


# Plot the age distribution in each country -------------------------------

# Count of observations in each age cat and year
counts <- all_dat %>%
  group_by(iso, year, age_turned) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(country = countrycode(iso, "iso3c", "country.name"))

# Create the faceted stacked bar plot
ggplot(counts, aes(x = year, y = count, fill = as.factor(age_turned))) +
  geom_bar(stat = "identity", position = "stack") +  # Use position = "stack" to show the total weighted counts
  facet_wrap(~ country, scales = "free_y", ncol = 5) +  # Create a facet for each country
  labs(x = "Year", y = "Number of Observations", fill = "Age Turned") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("figures2/age_dist.jpeg", height = 18, width = 15)

# Plot the rural/urban distribution in each country -----------------------

# Count of observations in each rural/urban cat and year
counts <- all_dat %>%
  group_by(iso, year, rural) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(country = countrycode(iso, "iso3c", "country.name"))

# Create the faceted stacked bar plot
ggplot(counts, aes(x = year, y = count, fill = as.factor(rural))) +
  geom_bar(stat = "identity", position = "stack") +  # Use position = "stack" to show the total weighted counts
  facet_wrap(~ country, scales = "free_y", ncol = 5) +  # Create a facet for each country
  labs(x = "Year", y = "Number of Observations", fill = "Rural") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("figures2/rural_dist.jpeg", height = 18, width = 15)


# Missing birth plot -------------------------------------------------------

imp_colors <- c(
  "Month & Year Specified" = "#228B22",
  "Month & Age Specified, Year Missing" = "#4B9CD3",
  "Year & Age Specified, Month Missing" = "#00CED1",
  "Year Specified, Month & Age Missing" = "#8A2BE2",
  "Age Specified, Month & Year Missing" = "#FFD700",
  "Month Specified, Age & Year Missing" = "#FF8C00",
  "Month, Age & Year Missing" = "#DC143C"
)

cross_section <- all_dat %>%
  group_by(case_id) %>%
  slice(1) %>%  # Take the first row for each case_id group
  ungroup()

proportions <- cross_section %>%
  group_by(iso, birth_imp) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(iso) %>% 
  mutate(total = sum(count),
         proportion = count / total,
         country = countrycode(iso, "iso3c", "country.name"))

proportions <- proportions %>%
  mutate(birth_imp_label = case_when(
    birth_imp == 1 ~ "Month & Year Specified",
    birth_imp == 2 ~ "Month & Age Specified, Year Missing",
    birth_imp == 3 ~ "Year & Age Specified, Month Missing",
    birth_imp == 4 ~ "Year & Age Specified, Month Missing",
    birth_imp == 5 ~ "Year Specified, Month & Age Missing",
    birth_imp == 6 ~ "Age Specified, Month & Year Missing",
    birth_imp == 7 ~ "Month Specified, Age & Year Missing",
    birth_imp == 8 ~ "Month, Age & Year Missing",
    TRUE ~ as.character(birth_imp)  # Default case, keep original if not matched
  ))

ggplot(proportions, aes(x = proportion, y = country, fill = birth_imp_label)) +
  geom_bar(stat = "identity", position = "stack") +  # Create a stacked bar plot
  labs(x = "Proportion of respondents", y = "", title = "a. Missing birth dates", 
       fill = "") +
  theme_minimal() +
  scale_fill_manual(values = imp_colors) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none") 
ggsave("figures2/missing_birth.jpeg", width = 3.4, height = 8)

# Missing marriage plot -------------------------------------------------------

proportions2 <- cross_section %>%
  filter(!is.na(mar_imp)) %>%
  group_by(iso, mar_imp) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(iso) %>% 
  mutate(total = sum(count),
         proportion = count / total,
         country = countrycode(iso, "iso3c", "country.name"))

proportions2 <- proportions2 %>%
  mutate(mar_imp_label = case_when(
    mar_imp == 1 ~ "Month & Year Specified",
    mar_imp == 2 ~ "Month & Age Specified, Year Missing",
    mar_imp == 3 ~ "Year & Age Specified, Month Missing",
    mar_imp == 5 ~ "Year Specified, Month & Age Missing",
    mar_imp == 6 ~ "Age Specified, Month & Year Missing",
    mar_imp == 7 ~ "Month Specified, Age & Year Missing",
    mar_imp == 8 ~ "Month, Age & Year Missing",
    TRUE ~ as.character(mar_imp)  # Default case, keep original if not matched
  ))

ggplot(proportions2, aes(x = proportion, y = country, fill = mar_imp_label)) +
  geom_bar(stat = "identity", position = "stack") +  # Create a stacked bar plot
  labs(x = "Proportion of respondents", y = "", title = "b. Missing marriage dates",
       fill = "") +
  theme_minimal() +
  scale_fill_manual(values = imp_colors) +
  theme(panel.grid.minor = element_blank())
ggsave("figures2/missing_marriage.jpeg", width = 6, height = 8)

