# Drought Child Marriage

This repo contains code for the following paper:
- Palmer, A.Y., Masuda, Y.J., Harou, A.P., Greene, M.E., Das, J.K., Bawah, A.A., Kwauk, C.T., MacDonald, G.K., Robinson, B.E., Baumgartner, J. and Koski, A. (2025) “The effect of drought on the rate of child marriage in 61 countries,” (Under Review).

## Code organisation
- `drought-child-marriage.Rproj` is the project file.
- `_run.R` is the main script that runs the entire analysis. *This is the only script that needs to be run by the user*.
- The directory `R` contains the functions that support the `_run.R` file
- The directory `data` contains the raw MICS and DHS survey files and other reference data. Specifically:
    - `data/emdat/emdat_drought_events_updated.xlsx` contains information on all drought events that aligned with data on child marriage. Additional variables have been added to indicate whether the drought event was excluded from the analysis and why. This spreadsheet was created based on the legacy database (available here: https://files.emdat.be/data/emdat_public_2023_09_25_full_legacy.xlsx) and was cross-checked with the current EM-DAT database.
    - `data/emdat/pend-gdis-1960-2018-disasterlocations_updated.xlsx` is the Geocoded Disasters (GDIS) Dataset available here: https://sedac.ciesin.columbia.edu/data/set/pend-gdis-1960-2018. Modifications have been made to the spreadsheet to correct some country codes and add additional region data not currently in the GDIS database. These entries are noted in blue at the end of the spreadsheet.
    - `data/emdat/gdis_adm2.xlsx` contains region information on drought events at the Adm2 level, created using information from the EM-DAT and GDIS database.

## Main outputs
- The directory `figures` contains all the figures included in the paper.
- The directory `results` contains the results across all countries in spreadsheet form.
  
## Instructions for reproducing the data
Running the code requires some management by the user. Specifically, the DHS and MICS data are not uploaded, nor cleaned in this repo. To reproduce the clean data, perform the following steps:
1. Go to https://github.com/annapal/child-marriage-regions and follow the instructions to reproduce the clean DHS/MICS datasets.
2. Copy all long data rds files into the directory `data/dhs-mics`. As a sanity check, the file path to the Afghanistan dataset should be `data/dhs-mics/AFG.rds`.
3. Open the `packages.R` file and make sure that all packages have been installed.
4. Source the `_run.R` file.

Please direct questions/comments/issues to anna.palmer@mail.mcgill.ca.

