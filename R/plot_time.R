
# Plot TEs over consecutive years of drought exposure

plot_time <- function(results_time) {
  
  # Remove missing values
  results_time <- na.omit(results_time)
  
  # Add country names
  results_time$country <- countrycode(results_time$iso, "iso3c", "un.name.en")
  
  # Set year of exposure to factor
  results_time$drought_yr <- factor(results_time$drought_yr, levels = c(3,2,1))
  
  # Plot
  p <- ggplot(results_time, aes(x = estimate, y = drought_yr)) +
    geom_point(size=1.5, shape=18) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0, linewidth = 0.5) +
    geom_vline(xintercept = 0, linewidth = 0.25) +
    labs(x = "Change in the prob. of marriage (95% CI)",
         y = "Years of consecutive exposure to drought") + 
    theme(panel.grid = element_blank(),
          panel.background = element_blank(),
          axis.ticks.y = element_line(linewidth = 0.25),
          axis.line.x = element_line(linewidth = 0.25),
          axis.line.y = element_line(linewidth = 0.25),
          axis.ticks.x = element_line(linewidth = 0.25),
          legend.key = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    xlim(-0.1, 0.1) +
    facet_wrap(~country, ncol=7,
               labeller = labeller(country = label_wrap_gen(width = 15)))
  
  # Save the plot
  ggsave(filename = "figures/te_time.jpeg", plot = p, height = 9, width = 10)
}
