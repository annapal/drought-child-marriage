
# Plot the main results

plot_main <- function(results_all, prop_country, prop_drought) {
  
  # Merge the data together
  results_all <- results_all %>%
    full_join(prop_country) %>%
    full_join(prop_drought)
  
  # Add country and geographic region
  results_all$country <- countrycode(results_all$iso, "iso3c", "country.name")
  results_all$region <- countrycode(results_all$iso, "iso3c", "region")
  
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
  
  # Plot average probability of marriage
  p1 <- ggplot(results_all, aes(x = ID, y = prob_mar)) +
    geom_bar(stat = "identity", fill="grey") +
    labs(y = "Average annual probability\n of marriage",
         x = NULL,
         title = "b.") + 
    coord_flip() +
    theme(
      panel.grid.major.y = element_line(color = "grey90", linewidth=0.5),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.background = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(linewidth = 0.25),
      axis.ticks.x = element_line(linewidth = 0.25),
      legend.key = element_blank(),
      plot.margin = margin(10, 3, 10, 3),
      plot.title = element_text(face = "bold"),
      axis.title.x = element_text(size = 8)
    ) +
    scale_x_continuous(breaks = results_all$ID, labels=NULL, limits = c(0.5, 70))
  
  # Plot the proportion of py exposed to drought
  p2 <- ggplot(results_all, aes(x = ID, y = prop_drought)) +
    geom_bar(stat = "identity", fill="grey") +
    labs(y = "Proportion of persion-years\n exposed to drought",
         x = NULL,
         title = "c.") + 
    coord_flip() +
    theme(
      panel.grid.major.y = element_line(color = "grey90", linewidth=0.5),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.background = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(linewidth = 0.25),
      axis.ticks.x = element_line(linewidth = 0.25),
      legend.key = element_blank(),
      plot.margin = margin(10, 10, 10, 3),
      plot.title = element_text(face = "bold"),
      axis.title.x = element_text(size = 8)
    ) +
    scale_x_continuous(breaks = results_all$ID, labels=NULL, limits = c(0.5, 70))
  
  # Plot the TEs
  p <- ggplot(results_all, aes(x = estimate, y = ID)) +
    geom_rect(aes(xmin = conf.low, xmax = conf.high, ymin = ID - 0.3, ymax = ID + 0.3), 
              fill = "#5B2C6F", alpha = 0.4) +  # Adjust color and transparency as needed
    geom_point(size = 1.5, shape = 16, color = "#5B2C6F") +
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
    scale_x_continuous(breaks = c(-0.04, -0.02, 0, 0.02, 0.04))
  
  # Get the locations for the region labels
  locations <- results_all %>%
    group_by(region) %>%
    summarize(max_ID = max(ID))
  
  # Add global region sub-headings
  p <- p +
    annotate("text",
             x = -0.0475,
             y = locations$max_ID + 1,
             label = locations$region,
             hjust = 1, fontface="bold", size=3) +
    coord_cartesian(xlim = c(-0.042, 0.04), 
                    clip = 'off')
  
  # Create subplots
  plot <- grid.arrange(p, p1, p2, ncol=3, widths=c(1.5, 0.5, 0.5), padding = unit(0, "lines"))
  
  # Save the plot
  ggsave(filename = "figures/main.jpeg", plot = plot, width = 8, height = 10, dpi = 300)
}
