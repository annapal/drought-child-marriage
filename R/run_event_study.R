
# Run "event-study" to assess PT and NA assumptions

run_event_study <- function(all_dat) {
  
  # Dataframe to store results
  results_es <- data.frame()
  
  for (i in unique(all_dat$iso)) {
    
    # Get data for country
    dat <- subset(all_dat, iso==i & drought2==0)
    
    # Run event study
    event_study <- feols(
      married_resid ~ i(drought_time, ref = -1)|
        cohort[age_turned, rural] + year[age_turned, rural],
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
  
  # Plot the coefficients
  ggplot(results_es, aes(x = year, y = estimate)) +
    geom_point(size=1, shape=16) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), linewidth = 0.5, width=0) +
    geom_hline(yintercept = 0, linewidth = 0.25, linetype="dotted") +
    labs(x = "Time to drought",
         y = "Coefficient") + 
    theme_minimal() +
    facet_wrap(~iso, scales = "fixed", ncol=8,
               labeller = labeller(iso = label_wrap_gen(width = 15)))
  
  # Save the figure
  ggsave("figures/event_study.jpeg", heigh=11, width=8)
}
