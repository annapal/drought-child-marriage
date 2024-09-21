
# Plot the main results

plot_main <- function(results_res, results_all) {
  
  # Add country and geographic region
  results_res$country <- countrycode(results_res$iso, "iso3c", "un.name.en")
  results_res$region <- countrycode(results_res$iso, "iso3c", "region")
  
  # Get order of countries
  results_all$region <- countrycode(results_all$iso, "iso3c", "region")
  results_all <- results_all %>% arrange(region, estimate)
  
  # Arrange results in specific order
  results_res <- results_res %>% arrange(match(iso, results_all$iso), rural)
  
  # Set plot location for each country
  results_res <- results_res %>%
    mutate(
      country_change = c(0, as.numeric(country[-1] != country[-n()])),
      country_cumsum = cumsum(country_change),
      region_change = c(0, as.numeric(region[-1] != region[-n()])),
      region_cumsum = cumsum(region_change),
      ID = 1 + country_cumsum + region_cumsum * 1.5 # Add a space between regions
    )
  
  # Add some space between the rural and urban estimates
  results_res$ID2 = results_res$ID + rep(c(-0.1, 0.1), nrow(results_all))
  
  # Plot the TEs
  p <- ggplot(results_res, aes(x = estimate, y = ID2, color = factor(rural))) +
    geom_point(size=1.5, shape=18) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0, linewidth = 0.5) +
    geom_vline(xintercept = 0, linewidth = 0.25, linetype="dotted") +
    labs(x = "Change in the probability of marriage (95% CI)",
         y = "") + 
    theme(panel.grid = element_blank(),
          panel.background = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.x = element_line(linewidth = 0.25),
          axis.ticks.x = element_line(linewidth = 0.25),
          legend.key = element_blank()) +
    scale_y_continuous(breaks=results_res$ID, labels=results_res$country) +
    xlim(-0.1, 0.1)
  
  # Get the locations for the region labels
  locations <- results_res %>%
    group_by(region) %>%
    summarize(max_ID = max(ID))
  
  # Add global region sub-headings
  p <- p +
    annotate("text",
             x = -0.0562,
             y = locations$max_ID + 1,
             label = locations$region,
             hjust = 1, fontface="bold", size=3) +
    coord_cartesian(xlim = c(-0.05, 0.05), 
                    clip = 'off')
  
  # Save the plot
  ggsave(filename = "figures/tes_main_rural.jpeg", plot = p, width = 6, height = 12, dpi = 300)
  
}