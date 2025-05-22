#' Compute the annual sum of growing degree days for each site and each year 
#' 
#' @param data_climate A list of dataframe for each site_id containing the value 
#'  of climate variables (the corrected mean temperature tascorrect) for each year and each month
#'  (output of `get_monthly_climate()` function)
#' 
#' @return A list of dataframe for each site_id containing the value of sgdd for each year 
#' 
compute_derived_variables <- function(data_climate) {
  
  data_derived <- setNames(vector("list", length(data_climate)), names(data_climate))
  
  # Compute sgdd for each site
  for (site in names(data_climate)) {
    
    data_derived[[site]] <- data_climate[[site]] %>%
      
      # Remove the two first years (used for computing aet2pet)
      dplyr::filter(year > min(year)+1) %>% 
      
      # Compute annual precipitations and mean annual temperatures
      dplyr::group_by(year) %>% 
      dplyr::summarise(
        temp_mean = mean(tascorrect),
        temp_month_min = min(tascorrect),
        temp_month_max = max(tascorrect),
        pr_sum = sum(pr),
        pr_mean = mean(pr),
        pr_month_min = min(pr),
        pr_month_max = max(pr)
      ) %>% 
      dplyr::ungroup()
    
  }
  
  data_derived
}
