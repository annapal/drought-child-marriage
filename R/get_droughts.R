
# Get relevant drought events from emdat legacy spreadsheet
# Creates emdat_drought_events.xlsx which can be edited
# Once edited, save spreadsheet as emdat_drought_events_updated.xlsx

get_droughts <- function(long_data) {

  # Read in EM-DAT legacy dataset
  emdat <- suppressWarnings(read_excel("data/emdat/emdat_public_2023_09_25_full_legacy.xlsx", skip = 6))
  
  # Get min and max year for each DHS/MICS dataset
  dhs_mics_dat <- map_df(names(long_data), function(iso) {
    dat <- long_data[[iso]] %>% filter(!is.na(Adm1))
    if (nrow(dat) > 0) {
      tibble(
        ISO = iso,
        min = min(dat$year) + 3,
        max = max(dat$year) - 3
      )
    } else {
      tibble()
    }
  })
  
  # Get relevant drought events
  emdat_subset <- emdat %>%
    filter(`Disaster Type`=="Drought"|`Disaster Subtype`=="Drought") %>%
    inner_join(dhs_mics_dat, by = "ISO") %>%
    filter(`Start Year`>=min & `Start Year`<=max) %>%
    filter(`Start Year` %in% 1980:2018)
  
  # Save drought spreadsheet
  write_xlsx(emdat_subset, "data/emdat/emdat_drought_events.xlsx")
  print("Spreadsheet saved in: data/emdat/emdat_drought_events.xlsx")
}
