#' Get monthly climate data from Chelsa database
#'
#' @param coords data.frame with n rows and 3 columns:
#' \itemize{
#'  \item{"id": }{Unique id of the point (character)}
#'  \item{"longitude": }{Longitude of the point (WGS84 - epsg:4326 - x coord) (double)}
#'  \item{"latitude": }{Latitude of the point (WGS84 - epsg:4326 - y coord) (double)}
#'  \item{"altitude": }{Altitude of the point in m (double) - Can be NA if unknown}
#' }
#' @param var Chelsa variable name to extract within c("pr", "pet", "tas", "tasmax", "tasmin") (character)
#' @param years positive integers - years on which to fetch monthly data
#' @param average_year if TRUE, years are averaged to obtain a single value for each month
#' @param as_list if TRUE, return a named list with a dataframe for each id, otherwise,
#'  rowbind all sites
#' @param folderpath_chelsa Folderpath where chelsa variables with rasters to extract are stored (character)
#' @param folderpath_lapserate Folderpath where rasters of lapserate to extract are stored (character)
#' @param filepath_worldclim_alt Filepath where WorldClim SRTM raster is stored(character)
#'
#' @author Nathéo Beauchamp, \email{beauchamp.natheo@@gmail.com}, \email{natheo.beauchamp@@inrae.fr}
#'
#' @return Monthly horizontal radiation (Hrad) and diffuse to global ratio (DGratio)
#'    averaged between start_year and end_year
#'
#' @import dplyr, purrr, tidyr, terra
#' 
#' @source Chelsa database https://chelsa-climate.org/
#' 
get_monthly_climate <- function(coords,
                                vars = "tas",
                                folderpath_chelsa = "S:/envicloud/chelsa/chelsa_V2/GLOBAL/monthly",
                                folderpath_lapserate = "S:/lapserate",
                                filepath_worldclim_alt = "S:/WorldClim/wc2.1_30s_elev.tif") {
  
  # Check if vars are available
  if(!all(vars %in% c("pr", "pet", "tas", "tasmax", "tasmin"))) stop("not good var")
  
  # Check if problems with year range (be careful, chelsa only between 1983 and 2018)
  if (sum(coords$year_max > 2018) > 0) stop("Maximum year for Chelsa climatic variables is 2018")
  if (sum(coords$year_min < 1983) > 0) stop("Minimum year for Chelsa climatic variables is 1983")
  
  # Search for all years to fetch for each site 
  data_years <- coords %>% 
    dplyr::mutate(n_years = year_max - (year_min-2) + 1) %>% 
    tidyr::uncount(n_years) %>% 
    dplyr::group_by(id) %>% 
    dplyr::mutate(year = seq(unique(year_min-2), unique(year_max))) %>% 
    dplyr::mutate(year_chelsa = if_else(year < 1983, 1983, year)) # The 2 years lag (for aet2pet) is set to the minimum year if it is lower (no problem for the two lag years to not have the true year)
    
  data_months <- data_years %>% 
    dplyr::mutate(month = 12) %>% 
    tidyr::uncount(month, .id = "month")
    
  years_to_fetch <- unique(data_years$year_chelsa)
  

  ### Extract climate data ----
  out <- sapply(vars,  
                function(x) setNames(vector("list", length(years_to_fetch)), years_to_fetch),
                USE.NAMES = T, simplify = F)
  
  print("Fetching climate data from Chelsa...")
  pb <- txtProgressBar(min = 0, max = length(years_to_fetch)*length(vars), style = 3)
  i <- 0
  
  for (var in vars) {
    folderpath_chelsa_var <- file.path(folderpath_chelsa, var)
    
    for (y in years_to_fetch) {
  
      # Get id for which we have to extract the given year
      coords_year <- data_years %>% 
        dplyr::filter(year_chelsa == y) %>% 
        dplyr::select(id, longitude, latitude) %>% 
        dplyr::distinct()

      # Extract stacks of month rasters for given year and var and add to list
      out[[var]][[as.character(y)]] <- extract_var_year(coords_year, var, y, 
                                                        folderpath_chelsa_var)
      
      # Set progress bar
      i <- i+1
      setTxtProgressBar(pb, value = i)
    }
  }
  close(pb)
  
  
  ### Correct temperatures ----
  if ("tas" %in% vars) {
    print("Correcting temperatures...")
    
    pb <- txtProgressBar(min = 0, max = length(years_to_fetch), style = 3)
    i <- 0
    
    # Extract lasperates
    for (y in years_to_fetch) {
      
      # Get id for which we have to extract the given year
      coords_year <- data_years %>% 
        dplyr::filter(year == y) %>% 
        dplyr::select(id, longitude, latitude) %>% 
        dplyr::distinct()
      
      # Extract stacks of month lapserates for given year, and add to list
      out[["lr"]][[as.character(y)]] <- extract_var_year(coords_year, "lr", y, 
                                                         folderpath_lapserate)
      
      # Set progress bar
      i <- i+1
      setTxtProgressBar(pb, value = i)
    }
    close(pb)
    
    # Extract world clim and bind it to coords
    coords$alt_wc <- extract_wc_altitude(coords, filepath_worldclim_alt)
    
    # Correct temperature with altitude and lapserate
    out[["tascorrect"]] <- purrr::map2(out[["tas"]], out[["lr"]],
                                       ~correct_tas_year(coords, .x, .y))
  }

  
  ### Prepare final dataset ----
  print("Preparing final dataset...")
  out <- out %>%

    # Bind years together
    purrr::map(~purrr::reduce(.x, left_join, by = "id")) %>% 
  
    # Rescale variables 
    # (because for memory saving, values are scaled and offset as integers within chelsa rasters)
    purrr::map2(names(.), ~rescale_climate_var(.x, .y)) %>% 
  
    # Longer the dataframe (adding year and month as variable)
    purrr::map(~.x %>% 
                 tidyr::pivot_longer(!id,
                                     names_pattern = "(.*)_(.*)_(.*)",
                                     names_to = c(".value", "month", "year_chelsa"))) %>% 
    
    # Bind fetched variables into a single dataframe
    purrr::reduce(left_join, by = c("id", "year_chelsa", "month")) %>%
    dplyr::mutate(year_chelsa = as.integer(year_chelsa),
                  month = as.integer(month)) %>% 
    
    # Bind the fetched chelsa years to the corresponding true years 
    # useful when there are the two years lag before 1989 (i.e. we want to fetch for the year 1989 or 1990)
    dplyr::right_join(data_months %>% dplyr::select(id, year, year_chelsa, month), 
                      by = c("id", "year_chelsa", "month")) %>% 
    dplyr::arrange(id, year, month)
  
  # Return as a list of site ----
  out <- sapply(unique(out$id), function(x) dplyr::filter(out, id==x),
                  simplify = F, USE.NAMES = T)

  return(out)
}


#' Extract chelsa values for a given variable and year at a given point
#'
#' @param coords data.frame with n rows and 3 columns:
#' \itemize{
#'  \item{"id": }{Unique id of the point (character)}
#'  \item{"longitude": }{Longitude of the point (WGS84 - epsg:4326 - x coord) (double)}
#'  \item{"latitude": }{Latitude of the point (WGS84 - epsg:4326 - y coord) (double)}
#' }
#' @param var Variable name to extract within c("pr", "pet", "tas", "tasmax", "tasmin", "lr") (character)
#' @param year Year to extract between 1979/1980 (depending on variable) and 2018 (integer)
#' @param folderpath Folderpath where rasters to extract are stored (character)
#'
extract_var_year <- function(coords, var, year,
                             folderpath) {
  
  # Get rasters for the given variable and year
  stacks <- read_stack_var_year(var, year, folderpath)
  
  # Extract values from rasters at coordinates in coords dataframe
  res <- terra::extract(stacks, coords[, c("longitude", "latitude")])
  
  # Check number of rows
  if(nrow(res) != nrow(coords)) stop("missing plots")
  
  # Bind plotcode and extracted values
  res <- cbind(id = coords$id, res %>% select(-ID))
  
  return(res)
}


#' Import chelsa rasters for a given variable and year at a given point
#'
#' @param var Variable name to extract within c("pr", "pet", "tas", "tasmax", "tasmin", "lr") (character)
#' @param year Year to extract between 1979/1980 (depending on variable) and 2018 (integer)
#' @param folderpath Folderpath where rasters to extract are stored (character)
#'
#' @source https://chelsa-climate.org/
#' 
read_stack_var_year <- function(var, year, folderpath){
  
  # Init months as character
  months <- c(paste0("0",1:9), 10:12)
  
  # Get rasters filenames within the folder
  rast_in_folder  <- list.files(folderpath, full.names = F)

  # Get names of rasters to import
  var_rast <- ifelse(var == "pet", "pet_penman", var)
  if (var_rast != "lr") {
    rast_to_import <- paste0("CHELSA_", var_rast, "_",months,"_",year, "_V.2.1.tif")
  } else {
    rast_to_import <- paste0("lr_", months, "_", year, ".tif")
  }

  
  # Check if all rasters to import are available within the folder
  if(!all(rast_to_import %in% rast_in_folder)) stop("error missing files")
  
  # Import the 12 rasters as a stack of raster
  layer_t <- terra::rast(file.path(folderpath,rast_to_import[1]))
  layers <- layer_t
  for (m in 2:12){
    layer_t <- terra::rast(file.path(folderpath, rast_to_import[m]))
    add(layers) <- layer_t
  }
  
  # Rename chelsa var by removing prefix and suffix
  if (var_rast != "lr") {
    names(layers) <- gsub("_V.2.1", "", gsub("CHELSA_", "", names(layers)))
  }
  
  return(layers)
}


#' Extract worldclim elevation values at a given point (30s resolution SRTM)
#'
#' @param coords data.frame with n rows and 3 columns:
#' \itemize{
#'  \item{"id": }{Unique id of the point (character)}
#'  \item{"longitude": }{Longitude of the point (WGS84 - epsg:4326 - x coord) (double)}
#'  \item{"latitude": }{Latitude of the point (WGS84 - epsg:4326 - y coord) (double)}
#' }
#' @param rast_fp Filepath of world clime 30s SRTM raster (character)
#'
#' @source https://www.worldclim.org/data/worldclim21.html
#' 
extract_wc_altitude <- function(coords, rast_fp) {
  
  # Get worldclim elevation raster
  rast <- terra::rast(rast_fp)
  
  # Extract values from rasters at coordinates in coords dataframe
  res <- terra::extract(rast, coords[, c("longitude", "latitude")])
  
  # Check number of rows
  if(nrow(res) != nrow(coords)) stop("missing plots")
  
  # Get values (vector of altitude for each coord)
  data_alt <- res[,2]
  
  return(data_alt)
}


#' Extract worldclim elevation values at a given point (30s resolution SRTM)
#'
#' @param coords data.frame with n rows and 3 columns:
#' \itemize{
#'  \item{"id": }{Unique id of the point (character)}
#'  \item{"longitude": }{Longitude of the point (WGS84 - epsg:4326 - x coord) (double)}
#'  \item{"latitude": }{Latitude of the point (WGS84 - epsg:4326 - y coord) (double)}
#' }
#' @param tas data.frame with n rows and 13 columns:
#' \itemize{
#'  \item{"id": }{Unique id of the point (character)}
#'  \item{"tas_month_year": }{Value of chelsa temperature at air surface for month 
#'    and year given in column name (double) - 12 months from 1 to 12}
#' }
#' @param lr data.frame with n rows and 13 columns:
#' \itemize{
#'  \item{"id": }{Unique id of the point (character)}
#'  \item{"lr_month_year": }{Value of computed local lapserate for month 
#'    and year given in column name (double) - 12 months from 1 to 12}
#' }
#'
correct_tas_year <- function(coords, tas, lr) {
  
  # Get month as character (e.g. 01 for January)
  months <- c(paste0("0", 1:9), 10:12)
  
  # Correct temperatures for each month using local monthy lapserate
  tas_correct <- coords %>% 
    dplyr::left_join(tas, by = "id") %>% 
    dplyr::left_join(lr, by = "id") %>% 
    tidyr::pivot_longer(!names(coords),
                        names_pattern = "(.*)_(.*)_(.*)",
                        names_to = c(".value", "month", "year")) %>% 
    dplyr::mutate(tascorrect = tas + lr * (altitude - alt_wc),
                  tascorrect = ifelse(is.na(tascorrect), tas, tascorrect)) %>% 
    tidyr::pivot_wider(names_from = c("month", "year"),
                       names_sep = "_",
                       values_from = c("tas", "lr", "tascorrect")) %>% 
    dplyr::select(id, contains("tascorrect_"))

  return(tas_correct)
}



#' Extract worldclim elevation values at a given point (30s resolution SRTM)
#'
#' @param df_climate data.frame with climatic variable to rescale 
#'  (must contain columns "var_", "var" being the given variable below)
#' @param var Chelsa variable to rescale 
#'  within c("pet", "pr", "tas", "tasmin", "tasmax", "lr", "tascorrect")
#'
#' @source Chelsa documentation
#'
rescale_climate_var <- function(df_climate, var) {
  
  # Define scale and offset coeff in raw for each variable (available in chelsa documentation)
  rescale_coef_list <- list(
    "pet"         = c(scale = 1,      offset = 0),       # In kg.m-2 = mm
    "pr"          = c(scale = 0.01,   offset = 0),       # In kg.m-2 = mm
    "tas"         = c(scale = 0.1,    offset = -273.15), # In Degree Celsius
    "tasmin"      = c(scale = 0.1,    offset = -273.15), # In Degree Celsius
    "tasmax"      = c(scale = 0.1,    offset = -273.15), # In Degree Celsius
    "lr"          = c(scale = 1,      offset = 0),       # In Degree Celsius per meter
    "tascorrect"  = c(scale = 0.1,    offset = -273.15)  # In Degree Celsius
  )
  
  # Params list
  if(!var %in% names(rescale_coef_list)) {stop("missing rescale coeffs")}
  
  # Get rescale coefs from the list above
  rescale_coef <- rescale_coef_list[[var]]
    
  # Rescale considered columns
  df_climate_rescaled <- df_climate %>%
    dplyr::mutate_at(vars(contains(paste0(var, "_"))), 
                     ~round(rescale_coef[["scale"]]*.+rescale_coef[["offset"]], digits = 5))
  
  return(df_climate_rescaled)
}