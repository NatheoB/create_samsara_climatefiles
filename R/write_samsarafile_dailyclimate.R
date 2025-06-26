#' Write Samsata file with daily climate
#' 
#' @param data_climate Output dataframe of `get_monthly_climate()`
#' @param data_rad Output dataframe of `get_monthly_rad()`
#' @param pet_monthlymean_mm_rege Mean of the Turc monthly PET (in mm) (used for Mithila's regeneration models)
#' @param output_folder Filepath of the output folder
#'
#' @return Return the filepath of the written file
#' 
write_samsarafile_dailyclimate <- function(data_climate, data_rad,
                                           pet_monthlymean_mm_rege = 48,
                                           output_folder) {
  
  ids <- names(data_climate)

  fps <- setNames(vector("list", length(ids)), ids)
  for (site in ids) {
    
    climate_file_day <- data_climate[[site]] %>% 
      dplyr::left_join(data_rad %>% dplyr::filter(id == site), by = "month") %>% 
      dplyr::select(year, month, Hrad, tascorrect, pet_penman = pet_penman, pr) %>%
      
      # Remove the two first years for each plot 
      # (corresponding to the two burning years for computing aet2pet)
      dplyr::filter(year >= min(year)+2) %>% 
      
      # Transform monthly dataframe into daily dataframe
      dplyr::mutate(n_days = days_in_month(lubridate::ym(paste(year, month, sep="_")))) %>% 
      tidyr::uncount(n_days, .remove = F) %>% 
      
      # Compute day of year: doy
      dplyr::group_by(year) %>% 
      dplyr::arrange(month) %>% 
      dplyr::mutate(doy = row_number()) %>% 
      
      # Compute day in month: day
      dplyr::group_by(year, month) %>% 
      dplyr::arrange(doy) %>% 
      dplyr::mutate(day = row_number()) %>% 
      dplyr::ungroup() %>% 
      
      # Transform monthly values into daily ones
      dplyr::arrange(year, month, day) %>% 
      dplyr::mutate(pr = pr/n_days,
                    pet_turc = pet_monthlymean_mm_rege/n_days,
                    pet_penman = pet_penman/n_days,
                    Hrad = Hrad/n_days) %>% 
      
      # Filter dataset
      dplyr::select(year, 
                    day = doy, 
                    tascorrect,
                    pr,
                    Hrad,
                    month,
                    pet_turc,
                    pet_penman)
    
    
    # Create .txt header
    header <- paste(
      "year", "day", 
      "temperature_C", "precipitation_mm",
      "globalRadiation_MJ/m2",
      "month_1_12", "pet_Turc_mm", "pet_PenmanMonteith_mm",
      sep="\t"
    )
    
    # Transform dataframe into string
    data_txt <- format_delim(climate_file_day, delim = "\t", col_names = F)
    data_txt <- paste0("#", header, "\n", data_txt)
    
    # Create folder of the site
    dir.create(file.path(output_folder, site), recursive = T, showWarnings = FALSE)
    
    # Write
    fps[[site]] <- file.path(output_folder, site, "samsara_daily_climate.txt")
    
    fileConn <- file(fps[[site]])
    writeLines(data_txt, fileConn)
    close(fileConn)
  }
  fps <- dplyr::bind_rows(fps, .id = "id")
  
  return(fps)
}
