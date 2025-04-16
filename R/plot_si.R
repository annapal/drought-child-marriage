
# Other plots for the SI

plot_si <- function(all_dat) {
  
  # Plot years of data & timing of droughts ------------------------------------
  
  # Get years of data
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
  
  # Get timing of droughts
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
  
  # Plot the data
  ggplot(summary_df, aes(y = country)) +
    geom_rect(aes(xmin = min_year, xmax = max_year, 
                  ymin = id - 0.2, ymax = id + 0.2, 
                  fill = "Years of Data"),  
              alpha = 1) +
    geom_point(data = drought_summary_df[drought_summary_df$drought == 1, ],
               aes(x = year, y = id, color = "Drought Event"),  
               size = 2) +  
    scale_y_discrete(limits = summary_df$country) +
    labs(x = "Year", y = "Country", fill = NULL, color = NULL) +  
    scale_fill_manual(values = "grey60") +  
    scale_color_manual(values = "black") +  
    theme_classic(base_size = 14) +
    theme(
      axis.text = element_text(size = 12, color = "black"),
      axis.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 12),
      legend.key.size = unit(0.5, "cm"),
      legend.spacing.x = unit(0.2, "cm"),
      panel.grid = element_blank(),  # Remove background grids
      axis.line = element_line(size = 0.5, color = "black")
    )
  
  # Save the plot
  ggsave("figures/datasets.jpeg", height = 12, width = 8)
  
  # Plot the age distribution in each country -------------------------------
  
  # Count of observations in each age cat and year
  counts <- all_dat %>%
    group_by(iso, year, age_turned) %>%
    summarise(count = n(), .groups = 'drop') %>%
    mutate(country = countrycode(iso, "iso3c", "country.name"))
  
  # Create the faceted stacked bar plot
  ggplot(counts, aes(x = year, y = count, fill = as.factor(age_turned))) +
    geom_bar(stat = "identity", position = "stack") +  
    facet_wrap(~ country, scales = "free_y", ncol = 5) +  
    labs(x = "Year", y = "Number of Observations", fill = "Age Turned") +  
    theme_classic(base_size = 14) + 
    theme(
      axis.text = element_text(size = 12, color = "black"),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      axis.title = element_text(size = 14, face = "bold"),
      strip.text = element_text(size = 12, face = "bold"),
      panel.grid = element_blank(),
      axis.line = element_line(size = 0.5, color = "black"),
      legend.position = "top",
      legend.key.size = unit(0.5, "cm")
    ) +
    scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e"))
  
  # Save the plot
  ggsave("figures/age_dist.jpeg", height = 18, width = 15)
  
  # Plot the rural/urban distribution in each country -----------------------
  
  # Count of observations in each rural/urban cat and year
  counts <- all_dat %>%
    group_by(iso, year, rural) %>%
    summarise(count = n(), .groups = 'drop') %>%
    mutate(country = countrycode(iso, "iso3c", "country.name"))
  
  # Create the faceted stacked bar plot
  ggplot(counts, aes(x = year, y = count, fill = as.factor(rural))) +
    geom_bar(stat = "identity", position = "stack") +  
    facet_wrap(~ country, scales = "free_y", ncol = 5) +  
    labs(x = "Year", y = "Number of Observations", fill = "Rural") +  
    theme_classic(base_size = 14) +
    theme(
      axis.text = element_text(size = 12, color = "black"),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      axis.title = element_text(size = 14, face = "bold"),
      strip.text = element_text(size = 12, face = "bold"),
      panel.grid = element_blank(),
      axis.line = element_line(size = 0.5, color = "black"),
      legend.position = "top",
      legend.key.size = unit(0.5, "cm")
    ) +
    scale_fill_manual(values = c("#1b9e77", "#d95f02"))
  
  # Save the plot
  ggsave("figures/rural_dist.jpeg", height = 18, width = 15)
  
  # Missing birth plot -------------------------------------------------------
  
  imp_colors <- c(
    "Month & Year Specified" = "#556B2F",
    "Month & Age Specified, Year Missing" = "#4682B4",
    "Year & Age Specified, Month Missing" = "#5F9EA0",
    "Year Specified, Month & Age Missing" = "#6A5ACD",
    "Age Specified, Month & Year Missing" = "#DAA520",
    "Month Specified, Age & Year Missing" = "#FF8C69",
    "Month, Age & Year Missing" = "#A52A2A"
  )
  
  # Get one observation per respondent
  cross_section <- all_dat %>%
    group_by(case_id) %>%
    slice(1) %>%
    ungroup()
  
  # Calculate the proportions in each category
  proportions <- cross_section %>%
    group_by(iso, birth_imp) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(iso) %>% 
    mutate(total = sum(count),
           proportion = count / total,
           country = countrycode(iso, "iso3c", "country.name"))
  
  # Label the proportions
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
      TRUE ~ as.character(birth_imp)
    ))
  
  # Plot the data
  ggplot(proportions, aes(x = proportion, y = country, fill = birth_imp_label)) +
    geom_bar(stat = "identity", position = "stack") +  
    labs(x = "Proportion of respondents", y = "", title = "a. Missing birth dates", fill = "") +  
    scale_fill_manual(values = imp_colors) +  
    theme_classic(base_size = 14) +
    theme(
      axis.text = element_text(size = 12, color = "black"),
      axis.title = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0),
      panel.grid = element_blank(),
      axis.line = element_line(size = 0.5, color = "black"),
      legend.position = "none"
    )
  
  # Save the plot
  ggsave("figures/missing_birth.jpeg", width = 5, height = 12)
  
  # Missing marriage plot -------------------------------------------------------
  
  # Calculate the proportions in each category
  proportions2 <- cross_section %>%
    filter(!is.na(mar_imp)) %>%
    group_by(iso, mar_imp) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(iso) %>% 
    mutate(total = sum(count),
           proportion = count / total,
           country = countrycode(iso, "iso3c", "country.name"))
  
  # Label the proportions
  proportions2 <- proportions2 %>%
    mutate(mar_imp_label = case_when(
      mar_imp == 1 ~ "Month & Year Specified",
      mar_imp == 2 ~ "Month & Age Specified, Year Missing",
      mar_imp == 3 ~ "Year & Age Specified, Month Missing",
      mar_imp == 5 ~ "Year Specified, Month & Age Missing",
      mar_imp == 6 ~ "Age Specified, Month & Year Missing",
      mar_imp == 7 ~ "Month Specified, Age & Year Missing",
      mar_imp == 8 ~ "Month, Age & Year Missing",
      TRUE ~ as.character(mar_imp)
    ))
  
  # Plot the data
  ggplot(proportions2, aes(x = proportion, y = country, fill = mar_imp_label)) +
    geom_bar(stat = "identity", position = "stack") +  
    labs(x = "Proportion of respondents", y = "", title = "b. Missing marriage dates", fill = "") +  
    scale_fill_manual(values = imp_colors) +  
    theme_classic(base_size = 14) +
    theme(
      axis.text = element_text(size = 12, color = "black"),
      axis.title = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0),
      panel.grid = element_blank(),
      axis.line = element_line(size = 0.5, color = "black")
    )
  ggsave("figures/missing_marriage.jpeg", width = 8.5, height = 12)
}
