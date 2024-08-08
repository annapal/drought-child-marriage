
# Plot coefficients for cohort linear trends

plot_pt_coefs <- function(unit_trends) {
  
  # A country name to the data
  unit_trends$country <- countrycode(unit_trends$iso, "iso3c", "un.name.en")
  
  # Plot the cohort-level coefficients and error bars
  ggplot(unit_trends, aes(x = coef, y = cohort)) +
    geom_point(size=1.5, shape=18) +
    geom_errorbarh(aes(xmin = lower, xmax = upper),
                   height = 0, linewidth = 0.5) +
    geom_vline(xintercept = 0, linewidth = 0.25) +
    labs(x = "Change in the prob. of marriage (95% CI)",
         y = "Cohort") + 
    theme(panel.grid = element_blank(),
          panel.background = element_blank(),
          axis.ticks.y = element_line(linewidth = 0.25),
          axis.line.x = element_line(linewidth = 0.25),
          axis.line.y = element_line(linewidth = 0.25),
          axis.ticks.x = element_line(linewidth = 0.25),
          legend.key = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    xlim(-0.05, 0.05) +
    facet_wrap(~country, scales = "free_y", ncol=7,
               labeller = labeller(country = label_wrap_gen(width = 15))) + 
    scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(min(x), (max(x) + 1) * 1.1)))))
  
  # Save the plot
  ggsave("figures/unit_level_trends.jpeg", height = 12, width = 10)
}

# Plot TEs with unit level trends

plot_lt <- function(results_lt, results_all) {
  
  # Add country name to the data
  results_lt$country <- countrycode(results_lt$iso, "iso3c", "un.name.en")
  results_all$country <- countrycode(results_all$iso, "iso3c", "un.name.en")
  
  # Indicator of result with/without LT
  results_lt$lt <- "With"
  results_all$lt <- "Without"
  
  # Combine results
  results_all <- rbind(results_lt, results_all)
  
  # Plot
  p <- ggplot(results_all, aes(x = estimate, y = lt)) +
    geom_point(size=1.5, shape=18) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0, linewidth = 0.5) +
    geom_vline(xintercept = 0, linewidth = 0.25) +
    labs(x = "Change in the prob. of marriage (95% CI)",
         y = "Linear Trend") + 
    theme(panel.grid = element_blank(),
          panel.background = element_blank(),
          axis.ticks.y = element_line(linewidth = 0.25),
          axis.line.x = element_line(linewidth = 0.25),
          axis.line.y = element_line(linewidth = 0.25),
          axis.ticks.x = element_line(linewidth = 0.25),
          legend.key = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    xlim(-0.05, 0.05) +
    facet_wrap(~country, ncol=7,
               labeller = labeller(country = label_wrap_gen(width = 15)))

  # Save th plot
  ggsave(filename = "figures/te_lt.jpeg", plot = p, height = 12, width = 10)
}
