#' Write climate report for each site
#'
#' @param sites_output_folder Foler path where the output of the sites are stored, 
#' the same as the `output_folder` argument of the function `create_samsarafiles_climate()`
#'
#' @return Create a .html report in `output_folder` of each site and return filepaths of created files
#'
#' @author Nathéo Beauchamp, \email{beauchamp.natheo@@gmail.com}, \email{natheo.beauchamp@@inrae.fr}
#' 
#' @import lubridate, dplyr, tidyr, purrr, terra, readr, httr
#' 
create_climate_reports <- function(sites_output_folder) {
  
  ids <- unique(vroom(file.path(sites_output_folder, "samsara_monthly_climate.csv"))$id)
  
  print("Creating climatic reports...")
  pb <- txtProgressBar(min = 0, max = length(ids), style = 3)
  i <- 0
  
  for (id in ids) {
    rmarkdown::render("R/write_climatic_report.Rmd", 
                      params = list(files_folder = file.path("../",sites_output_folder), 
                                    id_site = id),
                      output_dir = file.path(sites_output_folder, id),
                      output_file = paste("report_climate"),
                      quiet = T)
    
    # Set progress bar
    i <- i+1
    setTxtProgressBar(pb, value = i)
  }
  close(pb)
  
  return(file.path(sites_output_folder, id))
}