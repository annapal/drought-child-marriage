
prop_drought <- read_xlsx("results/prop_drought.xlsx")
results_all <- read_xlsx("results/all_tes.xlsx")

# Add country and geographic region
results_all$country <- countrycode(results_all$iso, "iso3c", "un.name.en")
results_all$region <- countrycode(results_all$iso, "iso3c", "region")

# Arrange results by point estimate & region
results_all <- results_all %>% arrange(region, estimate)

# Set plot location for each country
results_all <- results_all %>%
  mutate(
    region_change = c(0, as.numeric(region[-1] != region[-n()])),
    region_cumsum = cumsum(region_change),
    ID = row_number() + region_cumsum * 1.5 # Add a space between regions
  ) %>%
  select(-region_change, -region_cumsum)

# Merge with proportion exposed data
colnames(prop_drought) <- c("iso", "prop_exp")
results_all <- full_join(results_all, prop_drought)

results_all$ID2 <- results_all$ID+0.5

# Plot the proportions
p1 <- ggplot(results_all, aes(x = ID, y = prop_exp)) +
  geom_bar(stat = "identity", fill="steelblue") +
  labs(y = "Prop. of person-time exposed to drought",
       x = "") + 
  coord_flip() +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(linewidth = 0.25),
        axis.ticks.x = element_line(linewidth = 0.25),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        legend.key = element_blank()) +
  # scale_x_continuous(breaks=results_all$ID, labels=results_all$country) +
  ylim(0,1) +
  xlim(0.5, 70)

# Plot the TEs
p <- ggplot(results_all, aes(x = estimate, y = ID)) +
  geom_point(size=1.5, shape=18) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0, linewidth = 0.5) +
  geom_vline(xintercept = 0, linewidth = 0.25, linetype="dotted") +
  labs(x = "Change in the probability of marriage (95% CI)",
       y = "") + 
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(linewidth = 0.25),
        axis.ticks.x = element_line(linewidth = 0.25),
        legend.key = element_blank()) +
  scale_y_continuous(breaks=results_all$ID, labels=results_all$country,
                     limits=c(0.5,70))

# Get the locations for the region labels
locations <- results_all %>%
  group_by(region) %>%
  summarize(max_ID = max(ID))

# Add global region sub-headings
p <- p +
  annotate("text",
           x = -0.0562,
           y = locations$max_ID + 1,
           label = locations$region,
           hjust = 1, fontface="bold", size=3) +
  coord_cartesian(xlim = c(-0.05, 0.05), 
                  clip = 'off')

# Create subplots
plot <- grid.arrange(p, p1, ncol=2, widths=c(2,1))

# Save the plot
ggsave(filename = "figures/exposed_main.jpeg", plot = plot, width = 10, height = 12, dpi = 300)
