#' Write Samsata file with computed derived climate variables
#' 
#' @param plot_info Dataframe with information of each plot (id, coordinates, 
#'  years between which to fetch and compute climate,
#'  altitude, soil rooting depth and soil water holding capacity)
#' @param output_folder Filepath of the output folder
#'
#' @return Return the filepath of the written file
#' 
write_samsarafile_plotinfo <- function(plots_info, output_folder) {
  
  # Round swhc because do not need as much precision
  plots_info$swhc_mm <- round(plots_info$swhc_mm, 2)
  
  # Create output folder
  dir.create(output_folder, recursive = T, showWarnings = FALSE)
  
  # Write the file
  fp <- file.path(output_folder, "samsara_plot_info.csv")
  write.table(plots_info, fp, sep = ";", dec = ".", row.names = F)
  
  return(fp)
}