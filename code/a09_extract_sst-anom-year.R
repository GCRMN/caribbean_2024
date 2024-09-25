# 1. Required packages ----

library(tidyverse)
library(terra)

# 2. List of files to download ----

list_url <- data.frame(date = seq(from = ymd("1985-03-25"), to = ymd("2023-12-31"), by = "1 day")) %>% 
  mutate(year = year(date),
         date = str_remove_all(date, "-"),
         url = paste0("https://www.star.nesdis.noaa.gov/pub/socd/mecb/crw/data/5km/v3.1_op/nc/v1.0/daily/ssta/",
                      year,
                      "/ct5km_ssta_v3.1_",
                      date,
                      ".nc"),
         filename = str_split_fixed(url, "/", Inf)[,16])

# 3. Create the function to create a raster of mean SST anom per year ----

aggregate_raster <- function(year_i){
  
  # 1. Download files ----
  
  list_url_i <- list_url %>% 
    filter(year == year_i)
  
  for(i in 1:nrow(list_url_i)){
    
    if(file.exists(paste0("data/05_sst-anom/", list_url_i[i, "filename"])) == FALSE){
      
      # Use mode "wb" for windows otherwise issue to read the file with terra
      download.file(url = list_url_i[i, "url"],
                    destfile = paste0("data/05_sst-anom/", list_url_i[i, "filename"]), mode = "wb")
      
    }
    
  }
  
  # 2. Combine files ----
  
  ncdf_files <- list.files("data/05_sst-anom/", full.names = TRUE) %>% 
    as_tibble()
  
  data_raster <- rast(ncdf_files$value)
  
  # 3. Mean ----
  
  data_raster_mean <- mean(data_raster)
  
  writeRaster(x = data_raster_mean,
              filename = paste0("data/06_sst-anom_year/sst-anom_", year_i, ".tif"),
              overwrite = TRUE)

  # 4. Delete raw files ----
  
  file.remove(ncdf_files$value)
  
}

# 4. Map over the function ----

map(unique(list_url$year), ~aggregate_raster(year_i = .))
