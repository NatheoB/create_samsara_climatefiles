#' Write Samsata file with daily climate
#' 
#' @param data_rad Output dataframe of `get_monthly_rad()`
#' @param doy_start_gs Day of the year of the first day of the growing season
#' @param doy_end_gs Day of the year of the last day of the growing season
#' @param use_turbid_medium Consider crown as turbid medium in light interception ?
#' @param output_folder Filepath of the output folder
#'
#' @return Return the filepath of the written file
#' 
write_samsarafile_weather <- function(data_rad, 
                                      doy_start_gs = 1, 
                                      doy_end_gs = 365,
                                      use_turbid_medium = TRUE,
                                      output_folder = "output") {
  
  # Create string output
  df_str <- data_rad %>% 
    
    # Convert monthly radiation dataframe into a string and create final radiation string
    tidyr::unite(df_rad, c(month, Hrad, DGratio), sep = "\t") %>% 
    dplyr::group_by(id) %>%
    dplyr::summarise(df_rad = paste0(df_rad, collapse = "\n")) %>% 
    dplyr::mutate(str_rad = paste(
      "#Radiation data",
      "#Month Global  Diffus/G",
      df_rad,
      sep = "\n")) %>%
    
    # Add veget period final string
    dplyr::mutate(
      str_vegetperiod = paste0(
        "#Vegetation period", "\n",
        "#leaf_on_doy, leaf_off_doy : day of year (see the calendar, fc)", "\n",
        "leaf_on_doy = ", doy_start_gs, "\n",
        "leaf_off_doy = ", doy_end_gs, "\n")) %>% 
    
    # SamsaraLight params string
    dplyr::mutate(str_params = paste0(
      "#light model options", "\n",
      "turbid_medium = ", tolower(as.character(use_turbid_medium)), "\n",
      "trunk_interception = true", "\n", 
      "direct_angle_step = 15", "\n",
      "height_angle_min = 10", "\n",
      "#use_diffuse = true", "\n",
      "diffuse_angle_step = 15", "\n",
      "#SOC or UOC" , "\n",
      "soc = false", "\n",
      "GMT = -1"
    )) %>% 
    
    
    # Bind all string together to obtain final string
    tidyr::unite("out_str", 
                 str_params, str_vegetperiod, str_rad,
                 sep = "\n\n") %>% 
    
    # Select variables
    dplyr::select(id, out_str)
  
  # Write file
  fps <- setNames(vector("list", nrow(df_str)), df_str$id)
  for (site in df_str$id) {
    
    # Create folder of the site
    dir.create(file.path(output_folder, site), recursive = T, showWarnings = FALSE)
    
    # Write
    fps[[site]] <- file.path(output_folder, site, "samsara_weather_file.txt")
    
    fileConn <- file(fps[[site]])
    writeLines(df_str$out_str[df_str$id == site], fileConn)
    close(fileConn)
  }
  fps <- dplyr::bind_rows(fps, .id = "id")
  
}