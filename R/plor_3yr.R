
# Plot results for 3 consecutive years of drought

plot_3yr <- function(results_3yr) {
  
  # Add country name
  results_3yr$country <- countrycode(results_3yr$iso, "iso3c", "country.name")
  
  # Plot the coefficients
  p <- ggplot(results_3yr, aes(x = year, y = estimate)) +
    geom_point(size=1, shape=16) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), linewidth = 0.5, width=0) +
    geom_hline(yintercept = 0, linewidth = 0.25, linetype="dotted") +
    labs(x = "Year of Drought",
         y = "Change in the probability of marriage (95% CI)",
         title = "b. ") + 
    theme_minimal() + # Starts with a minimal theme
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(),
      axis.ticks.length = unit(3, "pt"),
      axis.ticks = element_line(color = "black", linewidth = 0.25),
      plot.title = element_text(face = "bold"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    ) +
    scale_x_continuous(limits = c(0, 4), breaks = c(1, 2, 3)) +
    facet_wrap(~country, scales = "fixed", ncol=6,
               labeller = labeller(country = label_wrap_gen(width = 15)))
  
  # Save the figure
  ggsave(filename = "figures/3yr.jpeg", plot = p, width = 6, height = 10, dpi = 300)
}


