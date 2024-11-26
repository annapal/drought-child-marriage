library(dplyr)
library(countrycode)
library(fixest)
library(sf)
library(geodata)
library(ggplot2)
library(readxl)

all_dat <- readRDS("data/all_dat.rds")

# Add GID_1 code
all_dat$Adm1 <- substring(all_dat$Adm1, 6)
all_gadm <- st_as_sf(gadm(unique(all_dat$iso), path="data", version="3.6")) %>%
  select(GID_0, GID_1, NAME_1) %>%
  st_drop_geometry()
colnames(all_gadm) <- c("iso", "GID_1", "Adm1")
all_dat <- left_join(all_dat, all_gadm)

# Resave data
saveRDS(all_dat, "data/all_dat_v2.rds")

# Get other data
aux_data <- readRDS("data/wealth_data.rds") %>%
  select(GID_1, weighted_avg_rwi) %>%
  full_join(readRDS("data/cropland_prop.rds")) %>%
  filter(GID_1!="NA")
prob_data <- read_xlsx("results/prop_region.xlsx") %>%
  rename(iso=iso3)

# Get affected region-years
drought_aff <- unique(all_dat[all_dat$drought2 == 1, c("iso", "Adm1", "GID_1", "year", "year_iso")])
drought_aff <- drought_aff %>% left_join(aux_data) %>%
  left_join(prob_data)

# Save drought affected data
saveRDS(drought_aff, "data/drought_aff.rds")

# RUN IN SERVER

# Plot RWI
results_all <- readRDS("results/results_rwi.rds")
results_all$lower <- results_all$Estimate - 1.96 * results_all$`Std..Error`
results_all$upper <- results_all$Estimate + 1.96 * results_all$`Std..Error`
results_all$rwi <- factor(results_all$rwi, levels = c("(-Inf, -0.03]", "(-0.03, -0.015]", "(-0.015, 0]", "(0, Inf)"))
ggplot(results_all, aes(x = rwi, y = Estimate)) +
  geom_point(size=1, shape=16) +
  geom_errorbar(aes(ymin = lower, ymax = upper), linewidth = 0.5, width=0) +
  geom_hline(yintercept = 0, linewidth = 0.25, linetype="dotted") +
  labs(x = "Category",
       y = "Change in the probability of marriage (95% CI)") + 
  theme_minimal() + # Starts with a minimal theme
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.ticks.length = unit(3, "pt"),
    axis.ticks = element_line(color = "black", linewidth = 0.25),
    plot.title = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  facet_wrap(~iso, scales = "fixed", ncol=6)
ggsave("figures/rwi.jpeg", width = 7, height = 9, dpi = 300)

# Plot Crop
results_all <- readRDS("results/results_crop.rds")
results_all$lower <- results_all$Estimate - 1.96 * results_all$`Std..Error`
results_all$upper <- results_all$Estimate + 1.96 * results_all$`Std..Error`
results_all$crop <- factor(results_all$crop, levels = c("(0, 0.05]", "(0.05, 0.15]", "(0.15, 0.25)"))
ggplot(results_all, aes(x = crop, y = Estimate)) +
  geom_point(size=1, shape=16) +
  geom_errorbar(aes(ymin = lower, ymax = upper), linewidth = 0.5, width=0) +
  geom_hline(yintercept = 0, linewidth = 0.25, linetype="dotted") +
  labs(x = "Category",
       y = "Change in the probability of marriage (95% CI)") + 
  theme_minimal() + # Starts with a minimal theme
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.ticks.length = unit(3, "pt"),
    axis.ticks = element_line(color = "black", linewidth = 0.25),
    plot.title = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  facet_wrap(~iso, scales = "fixed", ncol=6)
ggsave("figures/crop.jpeg", width = 7, height = 9, dpi = 300)

# Plot Prob
results_all <- readRDS("results/results_prob.rds")
results_all$lower <- results_all$Estimate - 1.96 * results_all$`Std..Error`
results_all$upper <- results_all$Estimate + 1.96 * results_all$`Std..Error`
results_all$prob <- factor(results_all$prob, levels = c("(0, 0.05]", "(0.05, 0.10]", "(0.10, 0.15)", "(0.15, 0.20)", "(0.20, 0.25)", "(0.25, 0.30)"))
ggplot(results_all, aes(x = prob, y = Estimate)) +
  geom_point(size=1, shape=16) +
  geom_errorbar(aes(ymin = lower, ymax = upper), linewidth = 0.5, width=0) +
  geom_hline(yintercept = 0, linewidth = 0.25, linetype="dotted") +
  labs(x = "Category",
       y = "Change in the probability of marriage (95% CI)") + 
  theme_minimal() + # Starts with a minimal theme
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.ticks.length = unit(3, "pt"),
    axis.ticks = element_line(color = "black", linewidth = 0.25),
    plot.title = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  facet_wrap(~iso, scales = "fixed", ncol=6)
ggsave("figures/prob.jpeg", width = 7, height = 9, dpi = 300)
