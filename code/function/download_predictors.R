download_predictors <- function(){
  
  require(googledrive)
  
  list_files <- googledrive::drive_ls(path = "GEE")
  
  download_i <- function(file_name){

      if(str_detect(file_name, "site-coords") == TRUE){
        
        drive_download(file = paste0("GEE/", file_name),
                       path = paste0("data/03_site-coords/", file_name),
                       overwrite = TRUE)
        
      }else{
        
        drive_download(file = paste0("GEE/", file_name),
                       path = paste0("data/08_predictors/", file_name),
                       overwrite = TRUE)
        
      }
      
    }

  if(length(list_files) != 0){
    
    map(list_files$name, ~download_i(file_name = .x))
    
  }
  
}
