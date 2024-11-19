library(did2s)

# Convert to factors
all_dat$Adm1 <- as.factor(all_dat$Adm1)
all_dat$year_iso <- as.factor(all_dat$year_iso)

i <- "AFG"

dat <- subset(all_dat, iso3==i)

mod_did2s <- did2s(data = dat, 
                   yname = "married", 
                   treatment = "drought2",
                   first_stage = ~ 1 | Adm1[rural] + year[rural], 
                   second_stage = ~ i(drought2, ref = 0), 
                   cluster_var = "Adm1", 
                   weights = "Denorm_Wt", 
                   bootstrap = FALSE, n_bootstraps = 250, 
                   verbose = TRUE)
