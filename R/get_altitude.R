#' Fetch altitude at 30m resolution of a point
#'
#' @param coords - data.frame with n rows and 4 columns:
#' \itemize{
#'  \item{"id": }{Unique id of the point (character)}
#'  \item{"longitude": }{Longitude of the point (WGS84 - epsg:4326 - x coord) (double)}
#'  \item{"latitude": }{Latitude of the point (WGS84 - epsg:4326 - y coord) (double)}
#' }
#' @param folderpath_srtm30 Folderpath where tiles of SRTM30 are stored (character)
#'
#' @return Return a data.frame with 1 row for each point and 2 columns
#' \itemize{
#'  \item{"id": }{Unique id of the point (character)}
#'  \item{"altitude": }{Altitude of the point (in meters) (double)}
#' }
#'
#' @source https://dwtkns.com/srtm30m/
#' @source https://www2.jpl.nasa.gov/srtm/
#'
get_altitude <- function(coords,
                         folderpath_srtm30 = "S:/SRTM30") {
  
  print("Fetching altitudes from SRTM30...")
  
  # Get the name of the SRTM30 tile 
  coords_srtm30_tile <- find_srtm30_tilenames(coords)

  # For all tilenames, import data
  tilenames <- unique(coords_srtm30_tile$tilename)
  altitudes_list <- vector(mode = "list", length = length(tilenames))

  ntiles <- length(tilenames)
  pb <- txtProgressBar(min = 0, max = ntiles, style = 3)
  
  for (i in seq_along(tilenames)) {
    
    # Get sites in the given tile
    tmp_coords <- coords_srtm30_tile %>% 
      dplyr::filter(tilename == tilenames[i])

    # Fetch srtm30 altitude data for the given tile
    altitudes_list[[i]] <- extract_srtm30_altitude(tmp_coords, tilenames[i],
                                                      folderpath_srtm30)
    
    # Set progress bar
    setTxtProgressBar(pb, value = i)
  }
  
  # Close progress bar
  close(pb)
  
  # Bind tiles and keep only altitudes
  altitudes <- dplyr::bind_rows(altitudes_list) %>% 
    dplyr::select(id, altitude)
  
  return(altitudes)
}


#' Get the name of the SRTM30 tile for each coord 
#'
#' @description Get name of the SRTM30 tile to extract the correct one according 
#' to point location
#'
#' @param coords - data.frame with n rows and 4 columns:
#' \itemize{
#'  \item{"id": }{Unique id of the point (character)}
#'  \item{"longitude": }{Longitude of the point (WGS84 - epsg:4326 - x coord) (double)}
#'  \item{"latitude": }{Latitude of the point (WGS84 - epsg:4326 - y coord) (double)}
#' }
#' 
#' @return data.frame with n rows and 4 columns:
#' \itemize{
#'  \item{"id": }{Unique id of the point (character)}
#'  \item{"longitude": }{Longitude of the point (WGS84 - epsg:4326 - x coord) (double)}
#'  \item{"latitude": }{Latitude of the point (WGS84 - epsg:4326 - y coord) (double)}
#'  \item{"tilename": }{Tile name (e.g. N45W012) (character)}
#' }
#' 
find_srtm30_tilenames <- function(coords) {
  
  coords %>% 
    dplyr::mutate(lat_card = ifelse(latitude >= 0 , "N", "S"),
                  lat_deg = floor(latitude),
                  long_card = ifelse(longitude >= 0, "E", "W"),
                  long_int = floor(longitude),
                  long_deg = paste0(long_int%/%100, long_int%/%10, long_int%%10)) %>% 
    tidyr::unite(tilename, c(lat_card, lat_deg, long_card, long_deg), sep="") %>% 
    dplyr::select(id, longitude, latitude, tilename)
  
}


#' Fetch srtm30 altitude data for coords in a given tile 
#'
#' @param coords - data.frame with 1 row for each point and 4 columns:
#' \itemize{
#'  \item{"id": }{Unique id of the point (character)}
#'  \item{"longitude": }{Longitude of the point (WGS84 - epsg:4326 - x coord) (double)}
#'  \item{"latitude": }{Latitude of the point (WGS84 - epsg:4326 - y coord) (double)}
#' }
#' @param tilename Name of the srtm30 tile in which points in coords belong (e.g. N45W012) (character)
#' @param folderpath_srtm30 Folderpath where tiles of SRTM30 are stored (character)
#'
#' @return Return a data.frame with 1 row for each point and 2 columns
#' \itemize{
#'  \item{"id": }{Unique id of the point (character)}
#'  \item{"altitude": }{Altitude of the point (in meters) (double)}
#' }
#'
extract_srtm30_altitude <- function(coords_srtm30_tile, 
                                    tilename,
                                    folderpath_srtm30) {
  # Check if all tiles are available
  tilepath <- paste0(tilename, ".SRTMGL1.hgt.zip")
  if (!tilepath %in% list.files(folderpath_srtm30)) {stop(paste("missing tile raster", tilepath))}
  
  # Import raster
  rast_tile <- terra::rast(file.path(folderpath_srtm30, tilepath))
  
  # Extract from coords
  res_tile <- terra::extract(rast_tile, coords_srtm30_tile[,c("longitude", "latitude")])
  
  # Check number of rows
  if(nrow(res_tile) != nrow(coords_srtm30_tile)) stop("missing plots")
  
  # Bind id and extracted values
  res_tile <- data.frame(
    id = coords_srtm30_tile$id,
    altitude = res_tile[,2])
  
  return(res_tile)
}