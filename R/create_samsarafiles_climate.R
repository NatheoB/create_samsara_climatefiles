#' Create and save Samsara daily climatic files
#'
#' @param coords data.frame with n rows and 3 columns:
#' \itemize{
#'  \item{"id": }{Unique id of the point (character)}
#'  \item{"longitude": }{Longitude of the point (WGS84 - epsg:4326 - x coord) (double)}
#'  \item{"latitude": }{Latitude of the point (WGS84 - epsg:4326 - y coord) (double)}
#'  \item{"altitude": }{Altitude of the point in m (double) - Can be NA if unknown}
#' }
#' @param output_folder Filepath of the folder where to save climatic files (character) 
#' @param derived_vars Boolean, if TRUE compute sgdd and aet2pet variables for each year, 
#'  and store it in a new output file
#'
#' @return Create files in `output_folder` and return filepaths of created files
#' \itemize{
#'  \item{samsara_plot_info.txt}{Information of the plot for Samsara2 (swhc, altitude)}
#'  \item{samsara_climate_day.txt}{Daily climate file with daily radiation and climate data used in 
#'    output for Samsara2 model}
#'  \item{samsara_climate_derived.txt}{If `derived_vars` is TRUE, yearly sgdd and aet2pet derived variables
#'    (computed in Samsara2 model for annual growth and mortality predictions)}
#' }
#'
#' @author Nathéo Beauchamp, \email{beauchamp.natheo@@gmail.com}, \email{natheo.beauchamp@@inrae.fr}
#' 
#' @import lubridate, dplyr, tidyr, purrr, terra, readr, httr
#' 
create_samsarafiles_climate <- function(coords,
                                        output_folder = "output",
                                        derived_vars = FALSE) {
  
  # Store output filepaths
  out_fps <- list()
  
  message("--- PLOT INFO ---")
  ### samsara_plot_info.txt ----
  # Information of the plot for Samsara2
  
  # Get altitude of plots (Server URL: //195.221.110.170/projets/)
  data_altitude <- get_altitude(coords[, c("id", "longitude", "latitude")],
                                folderpath_srtm30 = "S:/SRTM30")
  
  
  # Get soil water holding capacity
  data_swhc <- get_swhc(coords[, c("id", "longitude", "latitude", "rooting_depth_m")])
  
  
  # Update coords file
  coords_updated <- coords %>% 
    dplyr::select(-altitude, -rooting_depth_m, -swhc_mm) %>% 
    dplyr::left_join(data_altitude, by = "id") %>% 
    dplyr::left_join(data_swhc, by = "id")
  
  
  # Create info file for each site in coords
  out_fps[["info"]] <- write_samsarafile_plotinfo(coords_updated,
                                                  output_folder = "output")
  
  
  message("--- RADIATION ---")
  ### samsara_light_radiation.txt
  
  # Get radiations from PVGIS
  data_rad <- get_monthly_rad(coords, 
                              start_year = 2005,
                              end_year = 2020,
                              average_years = TRUE, 
                              as_list = FALSE)
  
  # Create radiation file for each site in coords 
  out_fps[["radiation"]] <- write_samsarafile_weather(data_rad,
                                                      output_folder = "output")
  
  
  message("--- DAILY CLIMATE ---")
  ### samsara_climate_day.txt ----
  # Daily climate file with daily radiation and climate data used in output for Samsara2 model
  
  # Get climate (Server URL: //195.221.110.170/projets/)
  data_climate <- get_monthly_climate(coords_updated, 
                                      vars = c("pr", "pet", "tas"),
                                      folderpath_chelsa = "S:/envicloud/chelsa/chelsa_V2/GLOBAL/monthly",
                                      folderpath_lapserate = "S:/lapserate",
                                      filepath_worldclim_alt = "S:/WorldClim/wc2.1_30s_elev.tif")
  
  # Create daily climatic file for each site in coords
  out_fps[["daily_climate"]] <- write_samsarafile_dailyclimate(data_climate, data_rad,
                                                               output_folder = "output")
  
  
  ### samsara_climate_derived.txt ----
  
  # Only if specified
  if (!derived_vars) {return(out_fps)}

  message("--- DERIVED CLIMATE ---")
  
  # Compute sgdd
  data_sgdd <- compute_sgdd(data_climate)
  
  # Compute aet2pet
  data_aet2pet <- compute_aet2pet(data_climate, data_swhc, data_altitude)
    
    
  # Create derived climatic file for each site in coords
  out_fps[["derived_climate"]] <- write_samsarafile_derivedclimate(data_sgdd, data_aet2pet)
  
  
  ### Return all the filepaths of created files ---
  return(fps)
}