#' Write Samsata file with monthly climate
#' 
#' @param data_climate Output dataframe of `get_monthly_climate()`
#' @param pet_monthlymean_mm_rege Mean of the Turc monthly PET (in mm) (used for Mithila's regeneration models)
#' @param output_folder Filepath of the output folder
#'
#' @return Return the filepath of the written file
#' 
write_samsarafile_monthlyclimate <- function(data_climate,
                                             output_folder) {
  
  # Bind data of each plot
  data_climate_all <- dplyr::bind_rows(data_climate) %>% 
    
    # Remove the two first years for each plot 
    # (corresponding to the two burning years for computing aet2pet)
    dplyr::group_by(id) %>% 
    dplyr::filter(year >= min(year)+2)
  
  # Create output folder
  dir.create(output_folder, recursive = T, showWarnings = FALSE)
  
  # Write the file
  fp <- file.path(output_folder, "samsara_monthly_climate.csv")
  write.table(data_climate_all, fp, sep = ";", dec = ".", row.names = F)
  
  return(fp)
}