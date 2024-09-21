
results_yr <- read_xlsx("results/tes_by_yr.xlsx") %>% drop_na()
results_yr$drought_yr <- as.character(results_yr$drought_yr)
results_all <- read_xlsx("results/all_tes.xlsx") %>% select(-drought2) %>%
  mutate(drought_yr = "combined") %>% filter(iso %in% results_yr$iso)

results <- full_join(results_all, results_yr) %>%
  arrange(iso, drought_yr)

wald_dat <- data.frame(iso=results_all$iso, wald=NA, p_val=NA)

for (i in 1:nrow(results_all)) {
  iso3 <- results_all$iso[i]
  point <- results[results$iso==iso3,"estimate"][[1]]
  var <- (results[results$iso==iso3,"std.error"]^2)[[1]]
  wald <- ((point[1]-point[4])^2/var[1]) + ((point[2]-point[4])^2/var[2]) + ((point[3]-point[4])^2/var[3])
  p_val <- pchisq(wald, df=1, lower.tail = FALSE)
  
  wald_dat$wald[i] <- wald
  wald_dat$p_val[i] <- p_val
  wald_dat$diff[i] <- point[4]
}

wald_dat$het <- ifelse(wald_dat$p_val<0.1, 1, 0)
wald_dat <- wald_dat %>% arrange(diff)

# Included countries
iso_inc <- wald_dat[wald_dat$het==1, "iso"]

# Plot included countries
results_plot <- results %>% filter(iso %in% iso_inc) %>%
  arrange(match(iso, iso_inc))
results_plot$country <- countrycode(results_plot$iso, "iso3c", "un.name.en")
results_plot$region <- countrycode(results_plot$iso, "iso3c", "region")

results_plot$country <- factor(results_plot$country)
results_plot$drought_yr <- factor(results_plot$drought_yr, levels = c("combined", "1", "2", "3"))

results_plot <- results_plot %>% filter(drought_yr!="combined")

ggplot(results_plot, aes(x = country, y = estimate, 
                         ymin = conf.low, ymax = conf.high, 
                         color = drought_yr)) +
  geom_pointrange(position = position_dodge(width = 0.3), size=0.2) + 
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") + 
  ylim(-0.1, 0.1) +
  labs(x = "", 
       y = "Change in the probability of marriage", 
       color = "Drought Year") + 
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_manual(values = c("#56B4E9", "#0072B2", "#013F63"))
ggsave("figures/drought-yr_selected.jpeg", width=9, heigh=6)
