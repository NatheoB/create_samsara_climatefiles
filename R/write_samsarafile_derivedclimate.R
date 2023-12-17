#' Write Samsata file with computed derived climate variables
#' 
#' @param data_sgdd Output dataframe of `compute_sgdd()`
#' @param data_aet2pet Output dataframe of `compute_aet2pet()`
#' @param output_folder Filepath of the output folder
#'
#' @return Return the filepath of the written file
#' 
write_samsarafile_derivedclimate <- function(data_sgdd, data_aet2pet,
                                             output_folder) {
  
  ids <- intersect(names(data_sgdd), names(data_aet2pet))
  
  fps <- setNames(vector("list", length(ids)), ids)
  for (site in ids) {
    
    # Bind sgdd and aet2pet
    data_derivedclimate <- dplyr::left_join(
      data_sgdd[[site]], data_aet2pet[[site]],
      by = "year"
    )
    
    # Create folder of the site
    dir.create(file.path(output_folder, site), recursive = T, showWarnings = FALSE)
    
    # Write
    fps[[site]] <- file.path(output_folder, site, "samsara_derived_climate.csv")
    
    write.table(data_derivedclimate, fps[[site]],
                sep = ";", dec = ".", row.names = F)
  }
  fps <- dplyr::bind_rows(fps, .id = "id")
  
  return(fps)
}