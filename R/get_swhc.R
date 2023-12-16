#' Get monthly climate data from Chelsa database
#'
#' @param coords data.frame with n rows and 3 columns:
#' \itemize{
#'  \item{"id": }{Unique id of the point (character)}
#'  \item{"longitude": }{Longitude of the point (WGS84 - epsg:4326 - x coord) (double)}
#'  \item{"latitude": }{Latitude of the point (WGS84 - epsg:4326 - y coord) (double)}
#' }
#'
#' @return Return a data.frame with 1 row for each site and 2 columns
#' \itemize{
#'  \item{"id": }{Unique id of the site (character)}
#'  \item{"swhc": }{Soil water holding capacity of the site (in mm)}
#' }
#'
get_swhc <- function(coords) {

  # Extract SoilGrids data ----
  sg_vars <- c("soc", "clay", "silt") # Not need sand because deduced from clay and silt percentages
  sg_depths <- c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm")
  sg_var_depth <- expand.grid(var = sg_vars, depth = sg_depths)

  print("Fetching soil data from SoilGrids...")
  data_sg <- vector("list", nrow(sg_var_depth))
  pb <- txtProgressBar(min = 0, max = nrow(sg_var_depth), style = 3)
  for (i in 1:nrow(sg_var_depth)) {
    data_sg[[i]] <- extract_soilgrids_var_depth(coords, 
                                                sg_var_depth[i,"var"], 
                                                sg_var_depth[i,"depth"])
    # Set progress bar
    setTxtProgressBar(pb, value = i)
  }
  close(pb)
  data_sg <- purrr::reduce(data_sg, left_join, by = "id")
  
  
  # Rescale SoilGrids ----
  data_sg <- rescale_soilgrids(data_sg)
  
  
  # Extract rooting depth ----
  data_rooting_depth <- coords %>% 
    tidyr::drop_na(rooting_depth_m) %>% 
    dplyr::select(id, rooting_depth_m)
  
  if (sum(is.na(coords$rooting_depth_m)) > 0) {
    data_rooting_depth <- dplyr::bind_rows(
      data_rooting_depth,
      extract_rooting_depth(coords %>% dplyr::filter(is.na(rooting_depth_m)))
    )
  }
  
  
  # Compute soil water holding capacity in proportion for each layer ----
  data_swhc_prop <- compute_swhc_prop(data_sg, sg_depths)
  
  # Compute soil water holding capacity
  data_swhc <- compute_swhc(data_swhc_prop, data_rooting_depth, sg_depths)
  
  return(data_swhc)
}


#' Extract soil variables from SoilGrids
#' 
#' @param coords data.frame with n rows and 3 columns:
#' \itemize{
#'  \item{"id": }{Unique id of the point (character)}
#'  \item{"longitude": }{Longitude of the point (WGS84 - epsg:4326 - x coord) (double)}
#'  \item{"latitude": }{Latitude of the point (WGS84 - epsg:4326 - y coord) (double)}
#' }
#' @param var Variable to extract among c("bdod", "cec", "cfvo", "clay", 
#'  "nitrogen", "phh2o", "sand", "silt", "soc", "ocd", "ocs")
#' @param depth Depth to extract among c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm")
#' @param rast_folderpath Folderpath of the SoilGrids rasters (online repository is at
#' "/vsicurl/https://files.isric.org/soilgrids/latest/data")
#' 
#' @return The values for each site
#'
#' @source Poggio et al. 2021
#'
extract_soilgrids_var_depth <- function(coords, 
                                        var, depth, 
                                        rast_folderpath = "/vsicurl/https://files.isric.org/soilgrids/latest/data")
{
  ### LOAD RASTER
  rast_filepath <- file.path(rast_folderpath, var, paste0(var, "_", depth, "_mean", ".vrt"))
  rast <- terra::rast(rast_filepath)
  
  ### CONVERT COORDS
  # Set wgs84 proj to long/lat coords
  coords_igh <- terra::vect(coords, geom = c("longitude", "latitude"), crs = "epsg:4326")
  
  # Convert long/lat coords into igh (homolosine goode) projection
  coords_igh <- terra::project(coords_igh, "+proj=igh +datum=WGS84 +no_defs +towgs84=0,0,0")
  
  # Convert SpatVector into a coords dataframe
  coords_igh <- terra::crds(coords_igh, df = TRUE)
  
  
  ### EXTRACT SOILGRIDS VALUES AND NAME COLUMNS
  data_sg <- terra::extract(rast, coords_igh)[-1] # First column is ID : we don't mind
  names(data_sg) <- paste(var,depth,"mean", sep="_")
  
  data_sg <- bind_cols(id = coords$id, data_sg)
  data_sg
}


#' Rescale SoilGrids variables
#'
#' @param data_sg Output from function `extract_soilgrids_var_depth()`
#' 
#' @return data_sg data.frame but with rescaled values
#'
rescale_soilgrids <- function(data_sg) {
  
  # Conversion factor (in SoilGrids doc)
  #' ## Properties
  #' 
  #' |Name     |Description                                                                        |Mapped units   | Conversion factor|Conventional units |
  #' |:--------|:----------------------------------------------------------------------------------|:--------------|-----------------:|:------------------|
  #' |bdod     |Bulk density of the fine earth fraction                                            |cg/cm^3        |               100|kg/dm^3            |
  #' |cec      |Cation Exchange Capacity of the soil                                               |mmol(c)/kg     |                10|cmol(c)/kg         |
  #' |cfvo     |Volumetric fraction of coarse fragments (> 2 mm)                                   |cm^3/dm^3 (vol per mil)  |         10|cm^3/100cm^3 (vol%)|
  #' |clay     |Proportion of clay particles (< 0.002 mm) in the fine earth fraction               |g/kg           |                10|g/100g (%)         |
  #' |nitrogen |Total nitrogen (N)                                                                 |cg/kg          |               100|g/kg               |
  #' |phh2o    |Soil pH                                                                            |pH*10          |                10|pH                 |
  #' |sand     |Proportion of sand particles (> 0.05 mm) in the fine earth fraction                |g/kg           |                10|g/100g (%)         |
  #' |silt     |Proportion of silt particles (= 0.002 mm and = 0.05 mm) in the fine earth fraction |g/kg           |                10|g/100g (%)         |
  #' |soc      |Soil organic carbon content in the fine earth fraction                             |dg/kg          |                10|g/kg               |
  #' |ocd      |Organic carbon density                                                             |hg/m^3         |                10|kg/m^3             |
  #' |ocs      |Organic carbon stocks                                                              |t/ha           |                10|kg/m^2             |
  #' 
  conversion_factor <- c(
    "bdod" = 100, "cec" = 10, "cfvo" = 10, "clay" = 10, 
    "nitrogen" = 100, "phh2o" = 10, "sand" = 10, "silt" = 10, 
    "soc" = 10, "ocd" = 10, "ocs" = 10
  )
  
  # Rescale each column of the soilgrids dataset
  data_sg_rescaled <- as.data.frame(sapply(names(data_sg %>% 
                                                   dplyr::select(contains(names(conversion_factor)))), 
                                           function(X) {
    # var is the first part of the colname (e.g. cec_0-5cm_mean)
    var <- strsplit(X, "_")[[1]][1]
    # value is the third part of the colname (e.g. cec_0-5cm_mean)
    value <- strsplit(X, "_")[[1]][3]
    
    if (value == "mean") {
      # Divide the raw value by the conversion factor for means
      round(data_sg[,X]/conversion_factor[[var]], digits = 5)
    } else if (value == "uncertainty") {
      # Divide by 10
      round(data_sg[,X]/10, digits = 5)
    }
    
  }))
  
  # Rebind id
  data_sg_rescaled <- cbind(id = data_sg$id, data_sg_rescaled)
  return(data_sg_rescaled)
}


#' Extract rooting depth from European Soil Database
#' 
#' @param coords data.frame with n rows and 3 columns:
#' \itemize{
#'  \item{"id": }{Unique id of the point (character)}
#'  \item{"longitude": }{Longitude of the point (WGS84 - epsg:4326 - x coord) (double)}
#'  \item{"latitude": }{Latitude of the point (WGS84 - epsg:4326 - y coord) (double)}
#' }
#' @param rast_filepath Filepath of the .rst raster (BE CAREFUL, you also need the .RDC file 
#'  on the same folder)
#'  
#' @return A dataframe with soil rooting depth for each site id
#'
#' @source Panagos et al. 2012
#' 
extract_rooting_depth <- function(coords, 
                                  rast_filepath = "data/STU_EU_DEPTH_ROOTS.rst") {
  
  # Get raster from filepath
  rast <- terra::rast(rast_filepath)
  
  ### CONVERT COORDS
  # Get coords in wgs84 proj = long/lat coords
  coords_wgs84 <- terra::vect(coords, geom = c("longitude", "latitude"), 
                              crs = "epsg:4326")
  
  # Convert long/lat coords into ETRS89-LAEA projection
  coords_etrs89laea <- terra::project(coords_wgs84, "epsg:3035")
  
  # Convert SpatVector into a coords dataframe
  coords_etrs89laea <- terra::crds(coords_etrs89laea, df = TRUE)
  
  ### EXTRACT ROOTING DEPTH
  data_rooting_depth <- terra::extract(rast, coords_etrs89laea)[2] # First column is ID : we don't mind
  names(data_rooting_depth) <- c("rooting_depth_m")
  data_rooting_depth <- bind_cols(id = coords$id, data_rooting_depth)
  
  return(data_rooting_depth)
}


#' Compute proportion of soil water for each depth layer
#' 
#' @param data_sg SRescald soilgrids soil data (see `extract_soilgrids_var_depth()`)
#' @param depths Depths on which to compute proportion of soil water
#' 
#' @return Return a data.frame with swhc proportion for each depth layer
#' 
#' @source https://bsssjournals.onlinelibrary.wiley.com/doi/full/10.1111/ejss.12192
#' 
compute_swhc_prop <- function(data_sg, depths) {
  
  swhc_prop <- sapply(depths, function(depth) {
    
    # Get variables for the given depth
    OC <- data_sg[[paste("soc", depth, "mean", sep = "_")]] / 1000 * 100 # soc in g/kg but OC in formula is in %
    Cl <- data_sg[[paste("clay", depth, "mean", sep = "_")]]
    Si <- data_sg[[paste("silt", depth, "mean", sep = "_")]]
    
    # Compute water content at field capacity (cm3cm-3)
    thetaFC <-
      0.2449 - 0.1887 * (1/(OC+1)) + 0.004527 * Cl + 0.001535 * Si + 0.001442 * Si * (1/(OC+1)) - 0.00005110 * Si * Cl + 0.0008676 * Cl * (1/(OC+1))
    
    # Compute water content at wilting point (cm3cm-3)
    thetaWP <-
      0.09878 + 0.002127* Cl - 0.0008366 * Si - 0.07670 *(1/(OC+1)) + 0.00003853 * Si * Cl + 0.002330 * Cl * (1/(OC+1)) + 0.0009498 * Si * (1/(OC+1))
    
    # Compute ASWC = thetaFC - thetaWP (in volumetric proportion = cm3cm-3)
    out <- data.frame(swcFCProp = thetaFC, swcWPProp = thetaWP, swhcProp = thetaFC - thetaWP)
    names(out) <- paste0(names(out), "_", depth)
    
    out
    
  }, simplify = F, USE.NAMES = T)
  
  bind_cols(id = data_sg$id, swhc_prop)
}


#' Compute soil water holding capacity
#' 
#' @param data_swhc_prop Dataframe with proportion of soil water for each depth layer
#'  (see `compute_swhc_prop()`)
#' @param data_rooting_depth Dataframe wth soil rooting depth (see `extract_rooting_depth()`)
#' @param depths depths on which to consider the soil water holding capacity
#' 
#' @return Dataframe with soil water holding capacity of each site
#' 
compute_swhc <- function(data_swhc_prop, data_rooting_depth, depths) {
  
  data_merged <- dplyr::left_join(data_swhc_prop, data_rooting_depth, by = "id")
  
  # For each soil layer
  for (depth_str in depths) {
    
    # Get min and max depth as integer vect from string (e.g. "0-5cm" --> [0,5])
    depths_int <- strsplit(depth_str, "-")[[1]]
    depths_int[[2]] <- stringr::str_sub(depths_int[[2]], end = -3)
    depths_int <- as.integer(depths_int)
    
    # Compute sg layer size
    layer_size_cm <- depths_int[2] - depths_int[1]
    
    # Compute proportion of the considered soilgrids layer covered by root 
    prop_with_roots <- pmin( pmax( (data_rooting_depth$rooting_depth_m - depths_int[1]) / 
                                     (depths_int[2] - depths_int[1]), 0), 1)

    # Compute total swhc in mm
    data_merged[[paste0("swhcMm_", depth_str)]] <- 
      data_merged[[paste0("swhcProp_", depth_str)]] * layer_size_cm * prop_with_roots * 10
    
  }
  
  # Add total swhc and swc over all soil layers
  swhc_mm <- rowSums(data_merged %>% dplyr::select(contains("swhcMm_")))
  
  data_rooting_depth %>% 
    dplyr::left_join(bind_cols(id = data_merged$id, swhc_mm = swhc_mm), by = "id")
  
  
}
