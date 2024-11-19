
gdp_dat <- read_xlsx("data/gdp_2018.xlsx") %>%
  select(`Country Code`, `2018 [YR2018]`)
colnames(gdp_dat) <- c("iso", "gdp")
gdp_dat$gdp <- as.numeric(gdp_dat$gdp)

ag_dat <- read_xlsx("data/employment_in_agri_2018.xlsx") %>%
  select(`Country Code`, `2018 [YR2018]`)
colnames(ag_dat) <- c("iso", "ag")
ag_dat$ag <- as.numeric(ag_dat$ag)

results_plot <- results_all %>%
  left_join(gdp_dat) %>%
  left_join(ag_dat)

ggplot(results_plot, aes(x = ag, y = estimate)) +
  geom_point(size = 3) +  # Add points
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  # Add error bars
  geom_text_repel(aes(label = iso), box.padding = 0.3, max.overlaps = 10) +  # Add non-overlapping labels
  labs(x = "GDP", y = "Estimate", title = "GDP vs Estimate with Confidence Intervals") +
  theme_minimal()
