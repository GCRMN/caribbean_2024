render_qmd <- function(area_i){
  
  require(rmarkdown)
  
  territory_name <- str_replace_all(str_replace_all(str_to_lower(area_i), " ", "-"),  "---", "-")
  
  file_name <- paste0("chapter_", territory_name, ".docx")
  
  nb_chapter <- data_area %>% filter(area == area_i) %>% select(nb) %>% pull()
  
  if(file.exists(paste0("doc/", file_name)) == FALSE){
    
    render("code/function/create_chapter_doc.qmd", 
           output_file = file_name,
           output_dir = "doc/",
           quiet = TRUE)
    
  }
  
}