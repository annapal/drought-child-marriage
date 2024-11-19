## Load packages
source("./packages.R")

## Load R files
lapply(list.files("./R", full.names = TRUE, recursive = TRUE), source)

# Prepare the data
# all_dat <- combine_data()
all_dat <- readRDS("data/all_dat.rds")

# Run the model
run_model(all_dat)