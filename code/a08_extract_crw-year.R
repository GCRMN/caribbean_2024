# 1. Required packages ----

library(tidyverse)
library(terra)

# 2. Create a function to download the file ----

download_crw_year <- function(type){
  
  if(type == "dhw_max"){
    
    for(i in 1986:2024){
      
      # Use mode "wb" for windows otherwise issue to read the file with terra
      download.file(url = paste0("https://www.star.nesdis.noaa.gov/pub/socd/mecb/crw/data/5km/v3.1_op/nc/v1.0/annual/ct5km_dhw-max_v3.1_",
                                 i,
                                 ".nc"	),
                    destfile = paste0("data/06_dhw_year/dhw_max_", i, ".nc"), mode = "wb")
      
    }
    
  }else if(type == "ssta_max"){
    
    for(i in 1985:2024){
      
      # Use mode "wb" for windows otherwise issue to read the file with terra
      download.file(url = paste0("https://www.star.nesdis.noaa.gov/pub/socd/mecb/crw/data/5km/v3.1_op/nc/v1.0/annual/ct5km_ssta-max_v3.1_",
                                 i,
                                 ".nc"	),
                    destfile = paste0("data/05_ssta_year/ssta_max_", i, ".nc"), mode = "wb")
      
    }
    
  }else if(type == "ssta_mean"){
    
    for(i in 1985:2024){
      
      # Use mode "wb" for windows otherwise issue to read the file with terra
      download.file(url = paste0("https://www.star.nesdis.noaa.gov/pub/socd/mecb/crw/data/5km/v3.1_op/nc/v1.0/annual/ct5km_ssta-mean_v3.1_",
                                 i,
                                 ".nc"	),
                    destfile = paste0("data/05_ssta_year/ssta_mean_", i, ".nc"), mode = "wb")
      
    }
    
  }
  
}

# 3. Map over the function ----

map(c("dhw_max", "ssta_max", "ssta_mean"), ~download_crw_year(type = .))
