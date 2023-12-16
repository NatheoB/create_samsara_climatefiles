#' Compute the annual sum of growing degree days for each site and each year 
#' 
#' @param data_climate A list of dataframe for each site_id containing the value 
#'  of climate variables (the corrected mean temperature tascorrect) for each year and each month
#'  (output of `get_monthly_climate()` function)
#' @param sgdd_threshold Value above which we start to count the degrees as growing degrees
#' 
#' @return A list of dataframe for each site_id containing the value of sgdd for each year 
#' 
compute_sgdd <- function(data_climate, sgdd_threshold = 5.5) {
    
  data_sgdd <- setNames(vector("list", length(names(data_climate))), names(data_climate))
  
  # Compute sgdd for each site
  for (site in names(data_climate)) {
    
    data_sgdd[[site]] <- data_climate[[site]] %>%
    
    # Remove the two first years (used for computing aet2pet)
      dplyr::filter(year > min(year)+1) %>% 
      
    # Get number of days for each month of the given year
      dplyr::mutate(
        date = as.Date(paste(year,month,1,sep="-"), "%Y-%m-%d"),
        n_days = lubridate::days_in_month(date)) %>% 
      
    # Get growing degrees for each month
      dplyr::mutate(
        gd = tascorrect - sgdd_threshold,
        gd = pmax(0, gd)) %>% 
      
    # Compute number of growing degree-days in each year
      dplyr::group_by(year) %>% 
      dplyr::summarise(sgdd = sum(gd*n_days))
    
  }
  
  data_sgdd
}
