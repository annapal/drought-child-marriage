
results_all <- read_xlsx("results/all_tes.xlsx") %>% select(-drought2) %>%
  mutate(rural = "combined")
results_res <- read_xlsx("results/tes_by_rural.xlsx")
results_res$rural <- ifelse(results_res$rural==1, "rural", "urban")

results <- full_join(results_all, results_res) %>%
  arrange(iso, rural)

wald_dat <- data.frame(iso=results_all$iso, wald=NA, p_val=NA)

for (i in 1:nrow(results_all)) {
  iso3 <- results_all$iso[i]
  point <- results[results$iso==iso3,"estimate"][[1]]
  var <- (results[results$iso==iso3,"std.error"]^2)[[1]]
  wald <- ((point[2]-point[1])^2/var[2]) + ((point[3]-point[1])^2/var[3])
  p_val <- pchisq(wald, df=1, lower.tail = FALSE)
  
  wald_dat$wald[i] <- wald
  wald_dat$p_val[i] <- p_val
  wald_dat$diff[i] <- point[1]
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

results_plot$country <- factor(results_plot$country, levels = unique(results_plot$country))

ggplot(results_plot, aes(x = country, y = estimate, 
                         ymin = conf.low, ymax = conf.high, 
                         color = factor(rural))) +
  geom_pointrange(position = position_dodge(width = 0.3), size=0.2) + 
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") + 
  ylim(-0.06, 0.06) +
  labs(x = "", 
       y = "Change in the probability of marriage", 
       color = "Sub-Group") + 
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("figures/rural-urban_selected.jpeg", width=9, heigh=6)
