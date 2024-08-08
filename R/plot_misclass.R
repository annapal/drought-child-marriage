
# Function that plots proportion of observations misclassified

plot_misclass <- function(prop_misclass) {
  
  # Get countries where data on moving is available
  dat_all2 <- subset(prop_misclass, !is.na(prop))
  
  # Add country name to dataset
  dat_all2$country <- factor(dat_all2$country, levels = rev(unique(dat_all2$country)))
  
  # Plot the proportions
  ggplot(dat_all2, aes(x = country, y = prop)) +
    geom_bar(stat = "identity") +
    labs(
      x = "Country",
      y = "Proportion"
    ) +
    coord_flip() +
    ylim(0,1) + 
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(), 
    )
  
  # Save plot
  ggsave("figures/prop_misclassified.jpeg", width = 6, height = 8, dpi= 600)
  
}

