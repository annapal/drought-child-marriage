
# Plot the results with and without the linear trend

plot_main_lt <- function(results_lt, results_main) {
  
  # Add country name
  results_lt$country <- countrycode(results_lt$iso, "iso3c", "country.name")
  results_main$country <- countrycode(results_main$iso, "iso3c", "country.name")
  
  # Indicator of result with/without LT
  results_lt$lt <- "With"
  results_main$lt <- "Without"
  
  # Combine results
  results_all <- rbind(results_lt, results_main)
  
  # Plot
  p <- ggplot(results_all, aes(x = estimate, y = lt)) +
    geom_point(size=1.5, shape=18) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0, linewidth = 0.5) +
    geom_vline(xintercept = 0, linewidth = 0.25, linetype="dotted") +
    labs(x = "Change in the prob. of marriage (95% CI)",
         y = "Linear Trend") + 
    theme_minimal() + # Starts with a minimal theme
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(),
      axis.ticks.length = unit(3, "pt"),
      axis.ticks = element_line(color = "black", linewidth = 0.25),
      plot.title = element_text(face = "bold"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
    ) +
    xlim(-0.05, 0.05) +
    facet_wrap(~country, scales = "fixed", ncol=7,
               labeller = labeller(country = label_wrap_gen(width = 15)))
  
  # Save the figure
  ggsave(filename = "figures/main_lt.jpeg", plot = p, width = 8, height = 12, dpi = 300)
}
