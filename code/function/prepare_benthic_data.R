prepare_benthic_data <- function(data){
  
  result <- data %>% 
    # 1. Sum of benthic cover per sampling unit (site, transect, quadrat) and category
    mutate(category = case_when(subcategory == "Macroalgae" ~ "Macroalgae",
                                subcategory == "Turf algae" ~ "Turf algae",
                                subcategory == "Coralline algae" ~ "Coralline algae",
                                TRUE ~ category)) %>% 
    filter(category %in% c("Hard coral", "Macroalgae", "Turf algae", "Coralline algae", "Other fauna")) %>% 
    group_by(datasetID, region, subregion, ecoregion, country, territory, area, locality, habitat, parentEventID,
             decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, eventID, category) %>% 
    summarise(measurementValue = sum(measurementValue)) %>% 
    ungroup() %>% 
    # 2. Summarise data at the transect level (i.e. mean of photo-quadrats)
    # This avoid getting semi-quantitative data (e.g. when there is only 10 points per photo-quadrat)
    group_by(datasetID, region, subregion, ecoregion, country, territory, area, locality, habitat, parentEventID,
             decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, category) %>% 
    summarise(measurementValue = mean(measurementValue)) %>% 
    ungroup() %>% 
    # 3. Remove values greater than 100 (unlikely but included to avoid any issues later)
    filter(measurementValue <= 100)

  return(result)
  
}
