#' Create and save Samsara daily climatic files
#'
#' @param coords data.frame with n rows and 3 columns:
#' \itemize{
#'  \item{"id": }{Unique id of the point (character)}
#'  \item{"longitude": }{Longitude of the point (WGS84 - epsg:4326 - x coord) (double)}
#'  \item{"latitude": }{Latitude of the point (WGS84 - epsg:4326 - y coord) (double)}
#'  \item{"altitude": }{Altitude of the point in m (double) - Can be NA if unknown}
#' }
#' @param create_weather If TRUE, create the weather file
#' @param create_climate_monthly If TRUE, create the monthly climate file
#' @param create_climate_daily If TRUE, create the daily climate file
#' @param create_climate_derived If TRUE, create the climate file with derived variables (sgdd and aet2pet)
#' @param pet_monthlymean_mm_rege Mean of the Turc monthly PET (in mm) (used for Mithila's regeneration models)
#' @param output_folder Filepath of the folder where to save climatic files (character) 
#'
#' @return Create files in `output_folder` and return filepaths of created files
#' \itemize{
#'  \item{samsara_plot_info.csv}{Information of the plot for Samsara2 (swhc, altitude)}
#'  \item{samsara_weather.txt}{Weather climate file used for SamsaraLight inputs (one per plot)}
##'  \item{samsara_climate_monthly.txt}{Monthly climate file with raw climatic data from Chelsa}
#'  \item{samsara_daily_climate.txt}{Daily climate file with daily radiation and climate data used in 
#'    output for Samsara2 model (one file per plot)}
#'  \item{samsara_climate_derived.txt}{Yearly sgdd and aet2pet derived variables
#'    (computed in Samsara2 model for annual growth and mortality predictions)}
#' }
#'
#' @author Nathéo Beauchamp, \email{beauchamp.natheo@@gmail.com}, \email{natheo.beauchamp@@inrae.fr}
#' 
#' @import lubridate, dplyr, tidyr, purrr, terra, readr, httr
#' 
create_samsarafiles_climate <- function(coords,
                                        create_weather = TRUE,
                                        create_climate_monthly = TRUE,
                                        create_climate_daily = TRUE,
                                        create_climate_derived = TRUE,
                                        pet_monthlymean_mm_rege = 48,
                                        input_folder = "S:",
                                        output_folder = "output") {

  # Store output filepaths
  out_fps <- list()

  ### samsara_plot_info.txt ----
  # Information of the plot for Samsara2
  message("--- PLOT INFO ---")
  
  # Get altitude of plots (Server URL: //195.221.110.170/projets/)
  data_altitude <- get_altitude(coords[, c("id", "longitude", "latitude")],
                                folderpath_srtm30 = file.path(input_folder, "SRTM30"))
  
  
  # Get soil water holding capacity
  data_swhc <- get_swhc(coords[, c("id", "longitude", "latitude", "rooting_depth_m")])
  
  
  # Update coords file
  coords_updated <- coords %>% 
    dplyr::select(-altitude, -rooting_depth_m, -swhc_mm) %>% 
    dplyr::left_join(data_altitude, by = "id") %>% 
    dplyr::left_join(data_swhc, by = "id")
  
  
  # Create info file for each site in coords
  out_fps[["plotinfo"]] <- write_samsarafile_plotinfo(coords_updated, output_folder)
  
  
  ### samsara_light_radiation.txt
  # Information about SamsaraLight radiation parameters
  if (create_weather) {
    message("--- WEATHER ---")
    
    # Get radiations from PVGIS
    data_rad <- get_monthly_rad(coords, 
                                start_year = 2005,
                                end_year = 2020,
                                average_years = TRUE, 
                                as_list = FALSE)
    
    # Create radiation file for each site in coords 
    out_fps[["weather"]] <- write_samsarafile_weather(data_rad, 
                                                      doy_start_gs = 1, doy_end_gs = 365,
                                                      use_turbid_medium = TRUE,
                                                      output_folder = output_folder)
  }
  
  # CLIMATE FILES
  if (create_climate_monthly | create_climate_daily | create_climate_derived) {
    message("--- CLIMATE ---")
    
    # Get climate raw data (Server URL: //195.221.110.170/projets/)
    data_climate <- get_monthly_climate(coords_updated,
                                        vars = c("pr", "pet", "tas"),
                                        folderpath_chelsa = file.path(input_folder, "envicloud/chelsa/chelsa_V2/GLOBAL/monthly"),
                                        folderpath_lapserate = file.path(input_folder, "lapserate"),
                                        filepath_worldclim_alt = file.path(input_folder, "WorldClim/wc2.1_30s_elev.tif"))
    
    
    ### samsara_climate_monthly.csv ----
    # Climate file with monthly raw climatic variables
    if (create_climate_monthly) {
      message("- Monthly climate -")
      
      out_fps[["climate_monthly"]] <- write_samsarafile_monthlyclimate(data_climate, output_folder)
    }
    
    ### samsara_daily_climate.txt ----
    # Daily climate file with daily radiation and climate data used in output for Samsara2 model
    if (create_climate_daily) {
      message("- Daily climate -")
      
      out_fps[["climate_daily"]] <- write_samsarafile_dailyclimate(data_climate, data_rad,
                                                                   pet_monthlymean_mm_rege,
                                                                   output_folder)
    }
    
    ### samsara_climate_derived.csv ----
    # Climate file with sgdd ans aet2pet computed variables from monthly climate
    if (create_climate_derived) {
      message("- Derived climate -")
      
      # Compute sgdd
      data_sgdd <- compute_sgdd(data_climate)
      
      # Compute aet2pet
      data_aet2pet <- compute_aet2pet(data_climate, data_swhc, data_altitude)
      
      # Compute classic derived variables 
      data_derived <- compute_derived_variables(data_climate)
      
      # Create derived climatic file for each site in coords
      out_fps[["climate_derived"]] <- write_samsarafile_derivedclimate(data_sgdd, data_aet2pet, data_derived,
                                                                       output_folder = output_folder)
    }
    
  }
  
  ### Return all the filepaths of created files ---
  return(out_fps)
}