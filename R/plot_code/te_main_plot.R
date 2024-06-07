
# Load the results data
results <- read_excel("results/all_tes.xlsx")
results$country <- countrycode(results$iso, "iso3c", "un.name.en")
results$region <- countrycode(results$iso, "iso3c", "region")

# Arrange results by point estimate & region
results <- results %>% arrange(region, estimate)

# Set plot location for each country
results <- results %>%
  mutate(
    region_change = c(0, as.numeric(region[-1] != region[-n()])),
    region_cumsum = cumsum(region_change),
    ID = row_number() + region_cumsum * 1.5 # Add a space between regions
  ) %>%
  select(-region_change, -region_cumsum)

# Plot the TEs
p <- ggplot(results, aes(x = estimate, y = ID)) +
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
        legend.key = element_blank(),
        axis.text = element_text(size=12)) +
  xlim(-0.05, 0.05) +
  scale_y_continuous(breaks=results$ID, labels=results$country)

# Get the locations for the region labels
locations <- results %>%
  group_by(region) %>%
  summarize(max_ID = max(ID))

# Add the annotations using plotly
p <- ggplotly(p)
p <- p %>% layout(annotations = list(x = -0.0545,
                           y = locations$max_ID+1,
                           text = paste("<b>", locations$region, sep=""),
                           xref = "x",
                           yref = "y",
                           showarrow = FALSE,
                           xanchor = "right"))

# Save plot
# TODO: orca is depreciated
orca(p, "figures/tes_main.jpeg", height = 1500)

