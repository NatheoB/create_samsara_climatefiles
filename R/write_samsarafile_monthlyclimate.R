#' Write Samsata file with monthly climate
#' 
#' @param data_climate Output dataframe of `get_monthly_climate()`
#' @param output_folder Filepath of the output folder
#'
#' @return Return the filepath of the written file
#' 
write_samsarafile_monthlyclimate <- function(data_climate, output_folder = "output") {
  
  ids <- names(data_climate)
  
  fps <- setNames(vector("list", length(ids)), ids)
  for (site in ids) {
    
    # Create folder of the site
    dir.create(file.path(output_folder, site), recursive = T, showWarnings = FALSE)
    
    # Write
    fps[[site]] <- file.path(output_folder, site, "samsara_monthly_climate.csv")
    
    write.table(data_climate[[site]], fps[[site]],
                sep = ";", dec = ".", row.names = F)
  }
  fps <- dplyr::bind_rows(fps, .id = "id")
  
  return(fps)
  
}