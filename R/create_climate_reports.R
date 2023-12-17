create_climate_reports <- function(output_folder) {
  
  ids <- list.files(output_folder)
  
  print("Creating climatic reports...")
  pb <- txtProgressBar(min = 0, max = length(ids), style = 3)
  i <- 0
  
  for (id in ids) {
    rmarkdown::render("RMD/write_climatic_report.Rmd", 
                      params = list(files_folder = file.path("..", output_folder, id)),
                      output_dir = file.path(output_folder, id),
                      output_file = paste("report_climate"),
                      quiet = T)
    
    # Set progress bar
    i <- i+1
    setTxtProgressBar(pb, value = i)
  }
  close(pb)
  
  return(file.path(output_folder, id))
}