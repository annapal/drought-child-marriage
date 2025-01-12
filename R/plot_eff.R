
# Plot treatment effect hetereogeneity

plot_eff <- function() {
  # Combine results data
  results <- bind_rows(
    read_xlsx("results/etwfe_main.xlsx") %>%
      select(estimate, conf.low, conf.high, iso) %>%
      mutate(type = "overall", plot_loc = 2),
    read_xlsx("results/etwfe_rural.xlsx") %>%
      filter(rural == 1) %>%
      select(estimate, conf.low, conf.high, iso) %>%
      mutate(type = "rural", plot_loc = 1),
    read_xlsx("results/etwfe_3yr.xlsx") %>%
      select(estimate, conf.low, conf.high, year, iso) %>%
      mutate(type = paste("year", year + 1), plot_loc = -year - 1) %>%
      select(-year)
  )
  
  # Add missing data for Morocco in the Middle East & North Africa region
  results <- results %>%
    bind_rows(
      tibble(
        iso = "MAR",
        country = "Morocco",
        region = "Middle East & North Africa",
        estimate = NA,
        conf.low = NA,
        conf.high = NA,
        type = paste0("year ", 1:3),
        plot_loc = -1:-3
      )
    )
  
  # Add country and region names
  results <- results %>%
    mutate(
      country = countrycode(iso, "iso3c", "country.name"),
      region = countrycode(iso, "iso3c", "region"),
      type = factor(type)
    )
  
  # Function to create and save regional plots
  plot_region <- function(region_name, file_name, title_text) {
    results_sub <- filter(results, region == region_name)
    
    ggplot(results_sub, aes(x = estimate, y = plot_loc, color = type)) +
      geom_point(size = 2.5, shape = 18) +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, linewidth = 0.5) +
      geom_vline(xintercept = 0, linewidth = 0.25, linetype = "dotted") +
      geom_hline(yintercept = 0, linewidth = 0.1, linetype = "solid") +
      labs(x = "Change in the rate of child marriage (95% CI)", y = "Estimate", title = title_text) +
      theme_minimal() +
      theme(
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(),
        axis.ticks.length = unit(3, "pt"),
        axis.ticks = element_line(color = "black", linewidth = 0.25),
        plot.title = element_text(face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        strip.text = element_text(hjust = 0),
        legend.position = "none"
      ) +
      xlim(-0.1, 0.1) +
      scale_y_continuous(breaks = results_sub$plot_loc, labels = results_sub$type) +
      ggforce::facet_wrap_paginate(~country, scales = "fixed", ncol = 10, nrow = 4, labeller = labeller(country = label_wrap_gen(width = 20))) +
      scale_color_manual(values = c("black", "darkred", "#3B4F66", "#2A3C54", "#172A3A"))
    
    ggsave(file_name, width = 12, height = 5)
  }
  
  # Create and save plots for each region
  plot_region("Sub-Saharan Africa", "figures/eff_ssa.jpeg", "Sub-Saharan Africa")
  plot_region("South Asia", "figures/eff_sa.jpeg", "South Asia")
  suppressWarnings(plot_region("Middle East & North Africa", "figures/eff_me.jpeg", "Middle East & North Africa"))
  plot_region("Latin America & Caribbean", "figures/eff_la.jpeg", "Latin America & Caribbean")
  plot_region("Europe & Central Asia", "figures/eff_ec.jpeg", "Europe & Central Asia")
  plot_region("East Asia & Pacific", "figures/eff_ea.jpeg", "East Asia & Pacific")
  
}

