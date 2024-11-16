
# Run "event-study" to assess PT and NA assumptions

run_event_study <- function(all_dat) {
  
  # Dataframe to store results
  results_es <- data.frame()
  
  for (i in unique(all_dat$iso)) {
    
    # Get data for country
    dat <- subset(all_dat, iso==i & drought2==0)
    
    # Convert Adm1 to factor
    dat$Adm1 <- as.factor(dat$Adm1)
    
    # Run event study
    event_study <- feols(
      married_resid ~ i(drought_time, ref = -1)|
        Adm1[age_turned, rural] + year[age_turned, rural],
      data=subset(dat, drought2==0),
      vcov=~Adm1, weights=~Denorm_Wt
    )
    
    # Tidy the results
    results <- broom::tidy(event_study, conf.int = TRUE)
    results$year <- as.numeric(substr(results$term, 15, 17))
    results$iso <- i
    
    # Save results to dataframe
    results_es <- rbind(results_es, results)
  }
  
  # Save the results
  write_xlsx(results_es, "results/es_results.xlsx")
  
  results_es$country <- as.factor(countrycode(results_es$iso, "iso3c", "country.name"))
  
  # Plot the coefficients
  p <- ggplot(results_es, aes(x = year, y = estimate)) +
    geom_point(size=1, shape=16) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), linewidth = 0.5, width=0) +
    geom_hline(yintercept = 0, linewidth = 0.25, linetype="dotted") +
    labs(x = "Time to drought",
         y = "Coefficient") + 
    theme_minimal() +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(),
      axis.ticks.length = unit(3, "pt"),
      axis.ticks = element_line(color = "black", linewidth = 0.25),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    ) +
    scale_x_continuous(limits = c(-5.5, -1.5), breaks = c(-5, -4, -3, -2)) +
    facet_wrap(~country, scales = "fixed", ncol=8,
               labeller = labeller(country = label_wrap_gen(width = 15)))
  
  # Save the figure
  ggsave("figures/event_study.jpeg", plot=p, height=11, width=8)
}
