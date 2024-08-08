# Drought Child Marriage

This repo contains code for the following paper:
- paper1

## Code organisation
- `drought-child-marriage.Rproj` is the project file.
- `_run.R` is the main script that runs the entire analysis. *This is the only script that needs to be run by the user*. The script is divided into 5 sections:
    1. EM-DAT drought data and corresponding GDIS data are loaded into the environment.
    2. Dataframes are created to store all the results of all countries.
    3. The analysis is iterated over all countries, saving the results at each step.
    4. The dataframes containing the results for all countries are saved.
    5. The results are plotted.
- The directory `R` contains the functions that support the `_run.R` file
- The directory `data` contains the raw MICS and DHS survey files and other reference data. Specifically:

