library(plotly)

# Load the results data
results <- read_excel("results/all_tes.xlsx")
results$country <- countrycode(results$iso, "iso3c", "un.name.en")
results$region <- countrycode(results$iso, "iso3c", "region")

# Arrange results by point estimate
results <- results %>% arrange(estimate)

# Get region names
regs <- unique(results$region)

results_sub <- list()
p <- list()
for (reg in regs) {
  
  res <- subset(results, region==reg)
  results_sub[[reg]] <- res
  
  res$id <- 1:nrow(res)

  # Plot the TEs
  p[[reg]] <- ggplot(res, aes(x = estimate, y = id)) +
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
    scale_y_continuous(breaks=res$id, labels=res$country) +
    xlim(-0.05, 0.05)
  }

# Heights of each subplot
hts <- as.vector(unlist(lapply(results_sub, nrow))/nrow(results))

# Annotations
annotations <- list(
  list(text = regs[1], showarrow = FALSE, xref='paper', yref='paper'),
  list(text = regs[2], showarrow = FALSE, xref='paper', yref='paper')
)

fig <- subplot(p, nrows = 6, heights = hts,
               shareX = TRUE, shareY = TRUE, margin = 0.01)
fig <- fig %>% layout(annotations = annotations)
fig

orca(fig, "figures/tes_main.jpeg", height = 1500)
