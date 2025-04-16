
# Run analysis with a unit-level linear trend to test PT assumption

run_pt_test <- function(all_dat) {
  
  # Data frames to store the results
  unit_trends <- data.frame()
  results_lt <- data.frame()
  
  for (i in unique(all_dat$iso)) {
    
    # Get data for a country
    dat <- subset(all_dat, iso3==i)
    
    # De-mean age & rural status
    dat <- dat %>%
      group_by(cohort, year) %>% # De-mean within each cohort and year
      mutate(age_dm = age_turned - weighted.mean(age_turned, Denorm_Wt),
             rural_dm = rural - weighted.mean(rural, Denorm_Wt)) %>%
      ungroup()
    
    # Run extended twfe model with unit-level linear time trend
    start_yr <- min(dat$year)
    dat$cohort <- as.numeric(substring(dat$cohort, 6))
    mod <- feols(married ~ drought2:i(cohort, i.year, ref=1, ref2=start_yr)/
                   (age_dm + rural_dm) +
                   i(cohort, year, ref=1) | # Add cohort-level linear trend
                   cohort[age_turned, rural] + year[age_turned, rural],
                 data=dat, vcov=~Adm1, weights=~Denorm_Wt,
                 mem.clean=TRUE, notes = FALSE)
    
    # Extract coefficients for unit-level linear trends
    res_pt <- cbind(mod$coefficients, confint(mod), mod$coeftable[,4])
    colnames(res_pt) <- c("coef", "lower", "upper", "p")
    res_pt2 <- res_pt[1:(max(dat$cohort)-1),]
    res_pt2$iso <- i
    res_pt2$cohort <- 2:(max(dat$cohort))
    rownames(res_pt2) <- NULL
    
    # Save unit-level effects
    unit_trends <- rbind(unit_trends, res_pt2)
    
    # Calculate main & save results
    res_main <- data.frame(aggregate(mod, c("^drought2(?!.*:(age_dm|rural_dm)$).*")))
    res_main$iso <- i
    results_lt <- rbind(res_main, results_lt)
  }
  
  # Save results
  colnames(results_lt) <- c("estimate", "std.error", "statistic", "p.value", "iso")
  results_lt$conf.low <- results_lt$estimate + qnorm(0.025)*results_lt$std.error
  results_lt$conf.high <- results_lt$estimate + qnorm(0.975)*results_lt$std.error
  write_xlsx(results_lt, "results/etwfe_main_lt.xlsx")
  write_xlsx(unit_trends, "results/etwfe_lt.xlsx")
}


