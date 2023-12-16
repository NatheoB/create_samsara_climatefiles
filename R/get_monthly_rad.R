#' @description Fetch monthly radiation data from PVGIS website (by API)
#'    between start and end year (limit years are from 2005 to 2020).
#'    Fetched variables are Hrad = horizontal plane irradiation and
#'    DGratio = ratio of diffuse to global radiation (in horizontal plane).
#'    
#'    ! YOU NEED AN INTERNET CONNECTION TO ACCESS THE DATA BY API !
#'    
#'
#' @param coords - data.frame with n rows and 4 columns:
#' \itemize{
#'  \item{"id": }{Unique id of the point (character)}
#'  \item{"longitude": }{Longitude of the point (WGS84 - epsg:4326 - x coord) (double)}
#'  \item{"latitude": }{Latitude of the point (WGS84 - epsg:4326 - y coord) (double)}
#' }
#' @param start_year positive integer between 2005 and 2020 - start year on which to fetch monthly data
#' @param end_year positive integer between 2005 and 2020 - end year on which to fetch monthly data
#' @param average_year if TRUE, years are averaged to obtain a single value for each month
#' @param as_list if TRUE, return a named list with a dataframe for each id, otherwise,
#'  rowbind all sites
#'
#' @return Monthly horizontal radiation (Hrad) and diffuse to global ratio (DGratio)
#'    averaged between start_year and end_year
#'
#' @source https://joint-research-centre.ec.europa.eu/pvgis-photovoltaic-geographical-information-system_en
#'
#' @import dplyr, readr, httr, purrr
#' 
get_monthly_rad <- function(coords,
                            start_year = 2005, 
                            end_year = 2020,
                            average_years = TRUE,
                            as_list = TRUE) {

  # Check for start and end years (PVGIS go from 2005 to 2020)
  if (start_year < 2005) stop("PVGIS data start from year 2005")
  if (end_year > 2020) stop("PVGIS data end at year 2020")
  if (start_year > end_year) stop("start_year must be lower or equal to end_year")

  # Check long/lat

  # Get number of coords
  ncoords <- nrow(coords)
  
  # Set progress bar
  print("Fetching radiation data from PVGIS...")
  pb <- txtProgressBar(min = 0, max = ncoords, style = 3)
  
  # Init time
  API_sum_time <- 0
  
  # PVGIS limit the catching rate at max 30 requests per seconds
  max_requests_per_sec <- 30
  dt_request_min <- 1/max_requests_per_sec
  tfetch <- Inf
  
  # Get data for all coordinates from non-interactive interface of PVGIS at a given maximum rate
  data_PVGIS_list <- list()
  for (i in 1:ncoords) {
    
    # Limit the number of get requests to the PVGIS server (sleep if dt < sleeping_time)
    if (tfetch < dt_request_min) Sys.sleep(dt_request_min - tfetch)
    
    # Get time of catching
    tfetch <- Sys.time()
  
    # Get the data for each month and year, as string format
    out <- tryCatch(expr = {
      
      # Make our request to the API
      res <- httr::GET(paste0("https://re.jrc.ec.europa.eu/api/v5_2/MRcalc?", # MRcalc = Monthly radiation tool
                              "lat=", coords$latitude[i], # lat latitude
                              "&lon=", coords$longitude[i], # lon = longitude
                              "&startyear=", start_year, # Start year on which to fetch monthly data
                              "&endyear=", end_year, # End year on which to fetch monthly data
                              "&horirrad=1", # horirrad : Output horizontal plane irradiation
                              "&d2g=1", # d2g : Output monthly values of the ratio of diffuse to global radiation (horizontal plane)
                              "&outputformat=basic")) # outputformat as "basic" = output without text as csv
      
      # Transform request in binary into string
      rawToChar(res$content)
      
    }, error = function(e) {
      print(e)
      return(NULL)
    })
    
    # Add data to general list of datasets
    data_PVGIS_list[[coords$id[i]]] <- out
    
    # Update sum time
    API_sum_time <- API_sum_time + as.double(Sys.time()-tfetch)
    
    # Set progress bar
    setTxtProgressBar(pb, value = i)
    
  }

  # Close progress bar
  close(pb)
  
  # Print mean time for getting one coordinates dataset
  # print(paste("Mean fetching time :", round(API_sum_time/ncoords*1000), "ms"))
  
  # print("Converting output into dataframe...")
  
  out <- data_PVGIS_list %>% 
    purrr::map2(names(.),
                
                # Convert list string output into a single data.frame (binded by row)
                ~read.table(text=.x,
                            col.names = c("year", "month", "Hrad", "DGratio")) %>% 
                  dplyr::mutate(id = .y) %>% 
                  
                  # Rename months with integers between 1 and 12, and arrange from January to December
                  dplyr::mutate(month = as.integer(
                    dplyr::recode(month,
                                  "Jan" = 1, "Feb" = 2, "Mar" = 3, "Apr" = 4,
                                  "May" = 5, "Jun" = 6, "Jul" = 7, "Aug" = 8,
                                  "Sep" = 9, "Oct" = 10, "Nov" = 11, "Dec" = 12))) %>%
                  dplyr::select(id, year, month, Hrad, DGratio) %>% 
                  dplyr::arrange(id, year, month))
    

  # Compute monthly mean of Hrad and DGratio between start and end years if specified
  if (average_years) {
    out <- out %>%
      purrr::map(~.x %>% 
                   dplyr::select(-year) %>%
                   dplyr::group_by(id, month) %>%
                   dplyr::summarize_all(mean))
  }

  
  # Unlist into a single dataframe if precised
  if (as_list) return(out)
  
  return(dplyr::bind_rows(out))
}

