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
  
  fps <- setNames(vector("list", nrow(plots_info)), plots_info$id)
  for (site in plots_info$id) {
    
    # Transform into string
    plot_info_str <- paste(names(plots_info), plots_info[plots_info$id == site, ],
                           sep = "\t", collapse = "\n")
    
    # Create folder of the site
    dir.create(file.path(output_folder, site), showWarnings = FALSE)
    
    # Write
    fps[[site]] <- file.path(output_folder, site, "samsara_plot_info.txt")
    
    fileConn <- file(fps[[site]])
    writeLines(plot_info_str, fileConn)
    close(fileConn)
  }
  fps <- dplyr::bind_rows(fps, .id = "id")
  
}