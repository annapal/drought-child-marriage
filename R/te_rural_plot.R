
# Load the results data
results <- read_excel("results/tes_by_rural.xlsx")
results$country <- countrycode(results$iso, "iso3c", "un.name.en")
results$rural <- factor(results$rural, levels = c(0, 1), labels = c("Urban", "Rural"))

# Plot
p <- ggplot(results, aes(x = estimate, y = rural)) +
  geom_point(size=1.5, shape=18) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0, linewidth = 0.5) +
  geom_vline(xintercept = 0, linewidth = 0.25) +
  labs(x = "Change in the prob. of marriage (95% CI)",
       y = "") + 
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.ticks.y = element_line(linewidth = 0.25),
        axis.line.x = element_line(linewidth = 0.25),
        axis.line.y = element_line(linewidth = 0.25),
        axis.ticks.x = element_line(linewidth = 0.25),
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlim(-0.1, 0.1) +
  facet_wrap(~country, ncol=7,
             labeller = labeller(country = label_wrap_gen(width = 15)))

ggsave(filename = "figures/te_rural.jpeg", plot = p, height = 12, width = 10)
