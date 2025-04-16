
plot_misclass <- function(all_dat) {

  # Return Adm1 names to normal
  all_dat$Adm1 <- substring(all_dat$Adm1, 6)
  
  # Create variable related to moving
  all_dat <- all_dat %>%
    mutate(
      # Create variable indicating if woman has moved region
      moved = case_when(
        is.na(al_lived) ~ "Unknown", # Unknown if woman moved
        al_lived==1 ~ "No", # Woman always lived in same location
        al_lived==0 & age-yrs_lived<13 ~ "No", # Woman moved prior to being at risk of child marriage
        al_lived==0 & Adm1==Prev_Adm1 ~ "No", # Woman has moved, but moved within same region
        al_lived==0 & is.na(Prev_Adm1) ~ "Unknown", # Woman has moved, but to unknown region
        Adm1!=Prev_Adm1 ~ "Yes"), # Woman moved to a different region
      # Create a variable with the correct Adm1 region where applicable
      Adm1_moved = case_when(
        moved=="No" ~ Adm1,
        moved=="Yes" ~ Prev_Adm1,
        TRUE ~ NA
      ))
  
  # Read in and organize drought panel data
  drought_panel_dat <- readRDS("data/drought_panel_dat.rds")
  drought_panel_all <- do.call(rbind, drought_panel_dat) %>%
    select(iso, Adm1, year, drought)
  colnames(drought_panel_all) <- c("iso", "Adm1_moved", "year", "drought_prev")
  
  # Get subset of data where moving is known
  data2 <- subset(all_dat, moved!="Unknown")
  
  # Merge data
  data2 <- left_join(data2, drought_panel_all)
  
  # Create variable showing misclassification of drought
  data2 <- data2 %>%
    mutate(drought_misclass = case_when(
      Adm1_moved=="Abroad" | Adm1_moved=="Other" ~ 1, # If they previously lived abroad, drought status is misclassified
      drought_prev==drought ~ 0,
      drought_prev!=drought ~ 1,
      TRUE ~ NA
    ))
  
  # Calculate proportion in each country
  proportions <- data2 %>%
    group_by(iso3) %>%
    summarize(
      prop_drought_misclass = mean(drought_misclass == 1)
    )
  
  # Add country name
  proportions$country <- countrycode(proportions$iso3, "iso3c", "country.name")
  proportions$country <- factor(proportions$country, levels = rev(unique(proportions$country)))
  
  # Plot the proportions
  ggplot(proportions, aes(x = country, y = prop_drought_misclass)) +
    geom_bar(stat = "identity", fill = "#1b9e77", color = "black") +  
    labs(
      x = "Country",
      y = "Proportion"
    ) +
    coord_flip() +
    scale_y_continuous(limits = c(0,1.05), expand = c(0, 0)) +
    theme_classic(base_size = 14) + 
    theme(
      axis.text = element_text(size = 12, color = "black"),
      axis.title = element_text(size = 14, face = "bold"),
      panel.grid = element_blank(), 
      axis.line = element_line(linewidth = 0.5, color = "black")
    )
  
  # Save plot
  ggsave("figures/prop_misclassified.jpeg", width = 6, height = 8, dpi= 600)
}
