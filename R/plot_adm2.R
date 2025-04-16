
# Plot the results of the Adm2 analysis

plot_adm2 <- function(results_adm2, results_all) {
  
  results_adm2 <- results_adm2 %>% select(estimate, std.error, statistic, p.value, iso, conf.low, conf.high)
  
  # Add country name
  results_adm2$country <- countrycode(results_adm2$iso, "iso3c", "un.name.en")
  results_all$country <- countrycode(results_all$iso, "iso3c", "un.name.en")
  
  # Get main results where country had Adm2 data
  results_all <- results_all %>% filter(iso %in% results_adm2$iso)
  
  # Indicator of Adm1 vs. Adm2
  results_adm2$level <- "Adm2"
  results_all$level <- "Adm1"
  
  # Combine results
  results_all <- rbind(results_adm2, results_all)
  
  # Plot
  p <- ggplot(results_all, aes(x = estimate, y = level)) +
    geom_point(size = 1.5, shape = 18, color = "black") +  # Black diamond points for clarity
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, 
                   linewidth = 0.5, color = "black") +  # Black horizontal error bars
    geom_vline(xintercept = 0, linewidth = 0.25, linetype = "dotted", color = "black") +  # Dotted reference line
    labs(x = "Change in the rate of child marriage", y = "Level") +  
    theme_classic(base_size = 14) +  # Nature-style clean theme
    theme(
      axis.text = element_text(size = 12, color = "black"),
      axis.ticks.length = unit(3, "pt"),
      axis.ticks = element_line(color = "black", linewidth = 0.25),
      axis.line = element_line(color = "black", linewidth = 0.25),
      plot.title = element_text(face = "bold"),
      strip.text = element_text(size = 12, face = "bold"),  # Bold facet titles
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)  # Rotate x-axis labels
    ) +
    xlim(-0.05, 0.05) +
    facet_wrap(~country, ncol = 1, 
               labeller = labeller(country = label_wrap_gen(width = 15)))
  
  
  p <- ggplot(results_all, aes(x = estimate, y = level)) +
    geom_point(size=1.5, shape=18) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0, linewidth = 0.5) +
    geom_vline(xintercept = 0, linewidth = 0.25) +
    labs(x = "Change in the rate of child marriage",
         y = "Level") + 
    theme(panel.grid = element_blank(),
          panel.background = element_blank(),
          axis.ticks.y = element_line(linewidth = 0.25),
          axis.line.x = element_line(linewidth = 0.25),
          axis.line.y = element_line(linewidth = 0.25),
          axis.ticks.x = element_line(linewidth = 0.25),
          legend.key = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    xlim(-0.05, 0.05) +
    facet_wrap(~country, ncol=1,
               labeller = labeller(country = label_wrap_gen(width = 15)))
  
  # Save the plot
  ggsave(filename = "figures/te_adm2.jpeg", plot = p, height = 8, width = 4)
}
