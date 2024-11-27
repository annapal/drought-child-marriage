
plot_lt <- function(unit_trends) {
  
  # Add country name
  unit_trends$country <- countrycode(unit_trends$iso, "iso3c", "country.name")
  
  # Plot the coefficients
  p <- ggplot(unit_trends, aes(x = coef, y = cohort)) +
    geom_point(size=1, shape=16) +
    geom_errorbar(aes(xmin = lower, xmax = upper), linewidth = 0.5, width=0) +
    geom_vline(xintercept = 0, linewidth = 0.25, linetype="dotted") +
    labs(x = "Cohort",
         y = "Coefficient") + 
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
    xlim(-0.05, 0.05) +
    facet_wrap(~country, scales = "free_y", ncol=6,
               labeller = labeller(country = label_wrap_gen(width = 15)))
  
  # Save the figure
  ggsave(filename = "figures/unit_lt.jpeg", plot = p, width = 6, height = 10, dpi = 300)
}