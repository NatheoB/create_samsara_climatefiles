#' Write Samsata file with computed derived climate variables
#' 
#' @param data_sgdd Output dataframe of `compute_sgdd()`
#' @param data_aet2pet Output dataframe of `compute_aet2pet()`
#' @param data_derived Output dataframe of `compute_derived_variables()`
#' @param output_folder Filepath of the output folder
#'
#' @return Return the filepath of the written file
#' 
write_samsarafile_derivedclimate <- function(data_sgdd, data_aet2pet, data_derived,
                                             output_folder) {
  # Bind data sgdd and aet2pet of each plot
  data_derivedclimate_all <- dplyr::bind_rows(data_sgdd, .id = "id") %>% 
    dplyr::full_join(dplyr::bind_rows(data_aet2pet, .id = "id"),
                     by = c("id", "year")) %>% 
    dplyr::full_join(dplyr::bind_rows(data_derived, .id = "id"),
                     by = c("id", "year"))
  
  # Create output folder
  dir.create(output_folder, recursive = T, showWarnings = FALSE)
  
  # Write the file
  fp <- file.path(output_folder, "samsara_derived_climate.csv")
  write.table(data_derivedclimate_all, fp, sep = ";", dec = ".", row.names = F)
  
  return(fp)
}