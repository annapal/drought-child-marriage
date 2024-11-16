
plot_rural <- function(results_all, results_rural) {

  results_all$type <- "total"
  results_rural$type <- "rural"
  
  results_rural <- results_rural %>% filter(rural==1)
  
  # Add country and geographic region
  results_all$country <- countrycode(results_all$iso, "iso3c", "country.name")
  results_all$region <- countrycode(results_all$iso, "iso3c", "region")
  results_rural$country <- countrycode(results_rural$iso, "iso3c", "country.name")
  results_rural$region <- countrycode(results_rural$iso, "iso3c", "region")
  
  # Arrange results by point estimate & region
  results_all <- results_all %>% arrange(region, estimate)
  
  # Set plot location for each country
  results_all <- results_all %>%
    mutate(
      region_change = c(0, as.numeric(region[-1] != region[-n()])),
      region_cumsum = cumsum(region_change),
      ID = row_number() + region_cumsum * 1.5 # Add a space between regions
    ) %>%
    select(-region_change, -region_cumsum)
  
  # Get ID locations in rural results
  results_rural <- results_rural %>% full_join(results_all[,c("iso", "ID")])
  
  # Merge results together
  results_plot <- results_rural %>%
    full_join(results_all)
  
  # Plot the TEs
  p <- ggplot(results_plot, aes(x = estimate, y = ID, fill=type)) +
    geom_rect(aes(xmin = conf.low, xmax = conf.high, ymin = ID - 0.3, ymax = ID + 0.3), alpha = 0.4) +
    geom_point(aes(x = estimate, y = ID, color=type), size = 1.5, shape = 16) +
    scale_color_manual(values = c("total" = "#008080", "rural" = "#FF7F50")) +
    scale_fill_manual(values = c("total" = "#008080", "rural" = "#FF7F50")) +
    geom_vline(xintercept = 0, linewidth = 0.25, linetype = "dotted") +
    labs(x = "Change in the probability\n of marriage (95% CI)", y = "",
         title = "a.") +
    theme(
      panel.grid.major.y = element_line(color = "grey90", linewidth=0.5),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.background = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(linewidth = 0.25),
      axis.ticks.x = element_line(linewidth = 0.25),
      legend.key = element_blank(),
      plot.margin = margin(10, 3, 10, 30),
      plot.title = element_text(face = "bold"),
      axis.title.x = element_text(size = 8)
    ) +
    scale_y_continuous(breaks = results_all$ID, labels = results_all$country,
                       limits=c(0.5,70)) +
    scale_x_continuous(breaks = c(-0.06, -0.04, -0.02, 0, 0.02, 0.04, 0.06))
  
  # Get the locations for the region labels
  locations <- results_all %>%
    group_by(region) %>%
    summarize(max_ID = max(ID))
  
  # Add global region sub-headings
  p <- p +
    annotate("text",
             x = -0.0575,
             y = locations$max_ID + 1,
             label = locations$region,
             hjust = 1, fontface="bold", size=3) +
    coord_cartesian(xlim = c(-0.07, 0.07), 
                    clip = 'off')
  
  # Save the plot
  ggsave(filename = "figures/rural.jpeg", plot = p, width = 5, height = 10, dpi = 300)
}
