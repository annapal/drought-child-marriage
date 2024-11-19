
run_model <- function(all_dat) {
  
  results_main <- data.frame()
  results_rural <- data.frame()
  results_3yr <- data.frame()
  
  for (i in unique(all_dat$iso)) {
    dat <- subset(all_dat, iso3==i)
    
    # De-mean age & rural status
    dat <- dat %>%
      group_by(Adm1, year) %>% # De-mean within each Adm1 and year
      mutate(age_dm = age_turned - weighted.mean(age_turned, Denorm_Wt),
             rural_dm = rural - weighted.mean(rural, Denorm_Wt)) %>%
      ungroup()
    
    # Run extended twfe model
    start_yr <- min(dat$year)
    dat$cohort <- as.numeric(substring(dat$cohort, 6))
    mod <- feols(married ~ drought2:i(cohort, i.year, ref=1, ref2=start_yr)/
                   (age_dm + rural_dm)|
                   cohort[age_turned, rural] + year[age_turned, rural],
                 data=dat, vcov=~Adm1, weights=~Denorm_Wt,
                 mem.clean=TRUE, notes = FALSE)
    
    # Calculate results
    res_main <- data.frame(aggregate(mod, c("^(?!.*:(age_dm|rural_dm)$).*")))
    res_main$iso <- i
    res_rural <- data.frame(aggregate(mod, c("([^:]*rural_dm$)")))
    res_rural$iso <- i
    
    # Save results
    results_main <- rbind(res_main, results_main)
    results_rural <- rbind(res_rural, results_rural)
    
    # Get years where drought exposed for 3 years
    years_3yr <- dat %>%
      filter(drought_3yr == 1) %>%
      select(cohort, year, drought_yr) %>%
      unique()
    
    if (nrow(years_3yr)>0) {
    
      # Calculate over time result
      res_3yr <- data.frame()
      for (k in c(0,1,2)) {
        yrs <- subset(years_3yr, drought_yr==k)
        cohort_year_pairs <- paste0("drought2:cohort::", yrs$cohort, ":year::", yrs$year)
        cohort_year_pattern <- paste0("^(?:", paste(cohort_year_pairs, collapse = "|"), ")$")
        res <- data.frame(aggregate(mod, agg = cohort_year_pattern))
        res$year <- k
        res_3yr <- rbind(res, res_3yr)
      }
      res_3yr$iso <- i
      results_3yr <- rbind(res_3yr, results_3yr)
    }
  }
  
  colnames(results_main) <- c("estimate", "std.error", "statistic", "p.value", "iso")
  results_main$conf.low <- results_main$estimate + qnorm(0.025)*results_main$std.error
  results_main$conf.high <- results_main$estimate + qnorm(0.975)*results_main$std.error
  write_xlsx(results_main, "results/etwfe_main.xlsx")
  
  colnames(results_rural) <- c("estimate", "std.error", "statistic", "p.value", "iso")
  results_rural$conf.low <- results_rural$estimate + qnorm(0.025)*results_rural$std.error
  results_rural$conf.high <- results_rural$estimate + qnorm(0.975)*results_rural$std.error
  write_xlsx(results_rural, "results/etwfe_rural.xlsx")
  
  colnames(results_3yr) <- c("estimate", "std.error", "statistic", "p.value", "year", "iso")
  results_3yr$conf.low <- results_3yr$estimate + qnorm(0.025)*results_3yr$std.error
  results_3yr$conf.high <- results_3yr$estimate + qnorm(0.975)*results_3yr$std.error
  write_xlsx(results_3yr, "results/etwfe_3yr.xlsx")
}
