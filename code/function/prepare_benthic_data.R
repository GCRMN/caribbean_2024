prepare_benthic_data <- function(data, remove_na_algae){
  
  # Main categories
  
  result_category <- data %>% 
    # 1. Filter taxonomic level
    filter(category %in% c("Hard coral", "Algae", "Other fauna")) %>% 
    # 2. Sum of benthic cover per sampling unit (site, transect, quadrat) and category
    group_by(datasetID, region, subregion, ecoregion, country, territory, area, locality, habitat, parentEventID,
             decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, eventID, category) %>% 
    summarise(measurementValue = sum(measurementValue)) %>% 
    ungroup() %>% 
    # 3. Sum of benthic cover per sampling unit (site, transect, quadrat) and category
    group_by(datasetID, region, subregion, ecoregion, country, territory, area, locality, habitat, parentEventID,
             decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, eventID, category) %>% 
    summarise(measurementValue = sum(measurementValue)) %>% 
    ungroup() %>% 
    # 4. Regenerate 0 values
    group_by(datasetID) %>% 
    complete(category,
             nesting(region, subregion, ecoregion, country, territory, locality,
                     habitat, parentEventID, decimalLatitude, decimalLongitude, verbatimDepth,
                     year, month, day, eventDate),
             fill = list(measurementValue = 0)) %>%
    ungroup() %>% 
    # 5. Average of benthic cover per transect (i.e. mean of photo-quadrats)
    # This avoid getting semi-quantitative data (e.g. when there is only 10 points per photo-quadrat)
    group_by(datasetID, region, subregion, ecoregion, country, territory, area, locality, habitat, parentEventID,
             decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, category) %>% 
    summarise(measurementValue = mean(measurementValue)) %>% 
    ungroup() %>% 
    # 6. Remove values greater than 100 (unlikely but included to avoid any issues later)
    filter(measurementValue <= 100) %>% 
    # 7. Remove datasetID among top predictors for a given category
    filter(!(category == "Algae" & datasetID %in% c("0015", "0104", "0151")))
  
  # Algae subcategories
  
  # Option 1 - Remove datasets for which at least one row is NA for an algae subcategory
  result_subcategory <- if(remove_na_algae == TRUE){
    
    data %>% 
      # 1. Filter taxonomic level
      filter(category == "Algae") %>% 
      # 2. Remove datasets for which at least one row is NA for an algae subcategory
      group_by(datasetID) %>%
      filter(any(is.na(subcategory)) == FALSE) %>% 
      ungroup() %>% 
      mutate(category = subcategory) %>% 
      # 3. Sum of benthic cover per sampling unit (site, transect, quadrat) and category
      group_by(datasetID, region, subregion, ecoregion, country, territory, area, locality, habitat, parentEventID,
               decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, eventID, category) %>% 
      summarise(measurementValue = sum(measurementValue)) %>% 
      ungroup() %>% 
      # 4. Regenerate 0 values
      group_by(datasetID) %>% 
      complete(category,
               nesting(region, subregion, ecoregion, country, territory, area, locality,
                       habitat, parentEventID, decimalLatitude, decimalLongitude, verbatimDepth,
                       year, month, day, eventDate),
               fill = list(measurementValue = 0)) %>%
      ungroup() %>% 
      # 5. Average of benthic cover per transect (i.e. mean of photo-quadrats)
      # This avoid getting semi-quantitative data (e.g. when there is only 10 points per photo-quadrat)
      group_by(datasetID, region, subregion, ecoregion, country, territory, area, locality, habitat, parentEventID,
               decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, category) %>% 
      summarise(measurementValue = mean(measurementValue)) %>% 
      ungroup() %>% 
      # 6. Remove values greater than 100 (unlikely but included to avoid any issues later)
      filter(measurementValue <= 100) %>% 
      # 7. Remove datasetID among top predictors for a given category
      filter(!(category == "Macroalgae" & datasetID %in% c("0091", "0079"))) %>% 
      filter(!(category == "Other fauna" & datasetID %in% c("0152")))
    
    # Option 2 - Don't remove datasets for which at least one row is NA for an algae subcategory
    }else{ 
    
    data %>% 
      # 1. Filter taxonomic level
      filter(category == "Algae" & subcategory != "Cyanobacteria") %>% 
      drop_na(subcategory) %>% 
      mutate(category = subcategory) %>% 
      # 2. Sum of benthic cover per sampling unit (site, transect, quadrat) and category
      group_by(datasetID, region, subregion, ecoregion, country, territory, area, locality, habitat, parentEventID,
               decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, eventID, category) %>% 
      summarise(measurementValue = sum(measurementValue)) %>% 
      ungroup() %>% 
      # 3. Regenerate 0 values
      group_by(datasetID) %>% 
      complete(category,
               nesting(region, subregion, ecoregion, country, territory, area, locality,
                       habitat, parentEventID, decimalLatitude, decimalLongitude, verbatimDepth,
                       year, month, day, eventDate),
               fill = list(measurementValue = 0)) %>%
      ungroup() %>% 
      # 4. Average of benthic cover per transect (i.e. mean of photo-quadrats)
      # This avoid getting semi-quantitative data (e.g. when there is only 10 points per photo-quadrat)
      group_by(datasetID, region, subregion, ecoregion, country, territory, area, locality, habitat, parentEventID,
               decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, category) %>% 
      summarise(measurementValue = mean(measurementValue)) %>% 
      ungroup() %>% 
      # 5. Remove values greater than 100 (unlikely but included to avoid any issues later)
      filter(measurementValue <= 100) %>% 
      # 6. Remove datasetID among top predictors for a given category
      filter(!(category == "Macroalgae" & datasetID %in% c("0091", "0079"))) %>% 
      filter(!(category == "Other fauna" & datasetID %in% c("0152")))
  
  }
  
  # Hard coral genera
  
  result_genera <- data %>% 
    # 1. Filter taxonomic level
    filter(category == "Hard coral") %>%
    filter(genus %in% c("Orbicella", "Acropora", "Porites")) %>% 
    mutate(category = genus) %>% 
    # 2. Sum of benthic cover per sampling unit (site, transect, quadrat) and category
    group_by(datasetID, region, subregion, ecoregion, country, territory, area, locality, habitat, parentEventID,
             decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, eventID, category) %>% 
    summarise(measurementValue = sum(measurementValue)) %>% 
    ungroup() %>% 
    # 3. Regenerate 0 values
    group_by(datasetID) %>% 
    complete(category,
             nesting(region, subregion, ecoregion, country, territory, area, locality,
                     habitat, parentEventID, decimalLatitude, decimalLongitude, verbatimDepth,
                     year, month, day, eventDate),
             fill = list(measurementValue = 0)) %>%
    ungroup() %>% 
    # 4. Average of benthic cover per transect (i.e. mean of photo-quadrats)
    # This avoid getting semi-quantitative data (e.g. when there is only 10 points per photo-quadrat)
    group_by(datasetID, region, subregion, ecoregion, country, territory, area, locality, habitat, parentEventID,
             decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, category) %>% 
    summarise(measurementValue = mean(measurementValue)) %>% 
    ungroup() %>% 
    # 5. Remove values greater than 100 (unlikely but included to avoid any issues later)
    filter(measurementValue <= 100)

  # Combine data
  
  result <- bind_rows(result_category, result_subcategory, result_genera)
  
  return(result)
  
}
