library(dplyr)
library(countrycode)
library(fixest)
library(sf)
library(geodata)

all_dat <- readRDS("data/all_dat.rds")

# Add GID_1 code
all_dat$Adm1 <- substring(all_dat$Adm1, 6)
all_gadm <- st_as_sf(gadm(unique(all_dat$iso), path="data", version="3.6")) %>%
  select(GID_0, GID_1, NAME_1) %>%
  st_drop_geometry()
colnames(all_gadm) <- c("iso", "GID_1", "Adm1")
all_dat <- left_join(all_dat, all_gadm)

# Get SSA countries
countries <- unique(all_dat$iso)
region_info <- countrycode(countries, origin = "iso3c", destination = "region")
countries_ssa <- countries[region_info == "Sub-Saharan Africa"]
# dat <- subset(all_dat, iso3%in%countries_ssa)
dat <- subset(all_dat, iso3=="ETH")

# De-mean age & rural status
dat <- dat %>%
  group_by(GID_1, year_iso) %>% # De-mean within each Adm1 and year
  mutate(age_dm = age_turned - weighted.mean(age_turned, Denorm_Wt),
         rural_dm = rural - weighted.mean(rural, Denorm_Wt)) %>%
  ungroup()

# Run extended twfe model
mod <- feols(married ~ drought2:i(GID_1, i.year_iso, ref="ETH.1_1", ref2="1979_ETH")/
               (age_dm + rural_dm)|
               GID_1[age_turned, rural] + year_iso[age_turned, rural],
             data=dat, vcov=~GID_1, weights=~Denorm_Wt,
             mem.clean=TRUE, notes = FALSE)

# Get other data
aux_data <- readRDS("data/wealth_data.rds") %>%
  rename(GID_1 = region_id) %>%
  full_join(readRDS("data/cropland_prop.rds")) %>%
  filter(GID_1!="NA")

# Get affected region-years
drought_aff <- unique(all_dat[all_dat$drought2 == 1, c("iso", "Adm1", "GID_1", "year", "year_iso")])
drought_aff <- drought_aff %>% left_join(aux_data)

# Get coefficient names (regex)
drought_aff_subset <- drought_aff %>% filter(weighted_rwi<(-0.3) & iso=="ETH")
cohort_year_pairs <- paste0("drought2:GID_1::", drought_aff_subset$GID_1, ":year_iso::", drought_aff_subset$year_iso)
cohort_year_pattern <- paste0("^(?:", paste(cohort_year_pairs, collapse = "|"), ")$")
aggregate(mod, cohort_year_pattern)
