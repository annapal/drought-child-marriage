

# Treatment Effects -------------------------------------------------------

# Load the results data
results <- read_excel("results/all_tes.xlsx")
results$country <- countrycode(results$iso, "iso3c", "un.name.en")

# Arrange results by point estimate
results <- results %>% arrange(estimate)

# Plot the TEs
ggplot(results, aes(x = estimate, y = reorder(country, estimate))) +
  geom_point(size=0.8) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0, linewidth = 0.5) +
  geom_vline(xintercept = 0, linewidth = 0.25) +
  labs(x = "Change in the prob. of marriage (95% CI)",
       y = "") + 
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(linewidth = 0.25),
        axis.ticks.x = element_line(linewidth = 0.25),
        legend.key = element_blank(),
        axis.text = element_text(size=12)) +
  xlim(-0.05, 0.05)

ggsave("figures/tes.jpeg", height = 12, width = 6)

# Unit-level linear trends ------------------------------------------------

# Load the results
lt_results <- read_excel("results/linear_unit_trends.xlsx")
lt_results$country <- countrycode(lt_results$iso, "iso3c", "un.name.en")

# Plot the coefficients and error bars
ggplot(lt_results, aes(x = coef, y = factor(cohort))) +
  geom_point(size=0.8) +
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
        # axis.text = element_text(size=12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlim(-0.05, 0.05) +
  facet_wrap(~country, scales = "free_y", ncol=7,
             labeller = labeller(country = label_wrap_gen(width = 15)))

ggsave("figures/unit_level_trends.jpeg", height = 12, width = 10)

# TEs with unit level trends ----------------------------------------------

# Load the results
lt_results_tes <- read_excel("results/tes_with_lt.xlsx")
lt_results_tes$country <- countrycode(lt_results_tes$iso, "iso3c", "un.name.en")

# Indicator of result with/without LT
lt_results_tes$lt <- "With"
results$lt <- "Without"

# Combine results
results_all <- rbind(lt_results_tes, results)

# Plot results together
ggplot(results_all, aes(x = estimate, y = reorder(country, estimate),
                    color = lt)) +
  geom_point(size=0.8, position = position_dodge(width=0.5)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0, linewidth = 0.5,
                 position = position_dodge(width=0.5)) +
  geom_vline(xintercept = 0, linewidth = 0.25) +
  labs(x = "Change in the prob. of marriage (95% CI)",
       y = "",
       color = "Unit-level trend") + 
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(linewidth = 0.25),
        axis.ticks.x = element_line(linewidth = 0.25),
        legend.key = element_blank(),
        axis.text = element_text(size=12)) +
  xlim(-0.05, 0.05)

ggsave("figures/tes_with_lt.jpeg", height = 12, width = 8)



