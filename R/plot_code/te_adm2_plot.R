
# Plot the analysis at the Adm2 level

plot_te_adm2 <- function() {

  # Load the results data
  results_adm2 <- read_excel("results/all_tes_adm2.xlsx")
  results_adm2$country <- countrycode(results_adm2$iso, "iso3c", "un.name.en")
  results <- read_excel("results/all_tes.xlsx") %>%
    filter(iso %in% results_adm2$iso)
  results$country <- countrycode(results$iso, "iso3c", "un.name.en")
  
  # Indicator of Adm1 vs. Adm2
  results_adm2$level <- "Adm2"
  results$level <- "Adm1"
  
  # Combine results
  results_all <- rbind(results_adm2, results)
  
  # Plot
  p <- ggplot(results_all, aes(x = estimate, y = level)) +
    geom_point(size=1.5, shape=18) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0, linewidth = 0.5) +
    geom_vline(xintercept = 0, linewidth = 0.25) +
    labs(x = "Change in prob. of marriage",
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
  
  # If the figures folder doesn't exist, create it
  if (!dir.exists("figures")) {
    dir.create("figures")
  }
  
  # Save the plot
  ggsave(filename = "figures/te_adm2.jpeg", plot = p, height = 6, width = 3)
  print("Plot of Adm2 analysis saved in: figures/te_adm2.jpeg")
}
