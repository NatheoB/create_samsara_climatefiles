#' Convert corrdinates from lambert93 to wgs84 (long/lat) system
#' 
#' @param coords data.frame with lambert coordinates columns (x and y coordinates)
#' @param xy_colnames 2-length string vector with names of columns of both x and y lambert coordinates
#' 
#' @return The same data.frame but with longitude and latitude columns
#' 
#' @import sf
#' 
convert_lambert_to_longlat <- function(coords, xy_colnames = c("x", "y")) {
  
  df <- st_as_sf(x = coords,                         
                 coords = colnames,
                 crs = 2154)
  
  df <- st_transform(df, crs = 4326)
  
  df[, c("longitude", "latitude")] <- st_coordinates(df$geometry)
  
  st_drop_geometry(df)
}