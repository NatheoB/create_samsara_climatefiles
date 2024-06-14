#####################################################
#####################################################

# Create climate files for Mithila's plots (rege model)

# Script from Natheo Beauchamp
# beauchamp.natheo@gmail.com
# 28/03/24

# Be careful, you need an internet connection
# And rasters are stored in INRAE server: //195.221.110.170/projets/
# Thus, you need to be connected to INRAE network (VPN or Ethernet)

#####################################################
#####################################################

# Import libraries
library(terra)
library(dplyr)
library(purrr)
library(lubridate)
library(readr)
library(httr)
library(rmarkdown)
library(ggplot2)
library(tidyr)
library(vroom)

# Source functions in R folder
message_source_files <- sapply(grep("R$", list.files("R", recursive = TRUE), value = TRUE), 
                               function(x) source(file.path("R", x)))

# Coordinates of the sites
output_folder <- "output/mithila"

data_raw <- vroom("data/mithila_plots/plotinfo_combined_mu.csv")

coords <- data_raw %>% 
  dplyr::select(id = pl_number,
                longitude = x_coord,
                latitude = y_coord,
                altitude = z_coord) %>% 
  dplyr::mutate(
    id = as.character(id),
    altitude = case_when(altitude == "na" ~ NA, TRUE ~ as.numeric(altitude)),
    year_min = 2015,
    year_max = 2018,
    rooting_depth_m = NA,
    swhc_mm = NA
  )
  

# Compute derived climate variables (sgdd and aet2pet)
fps_files <- create_samsarafiles_climate(coords,
                                         create_weather = FALSE,
                                         create_climate_monthly = TRUE,
                                         create_climate_daily = FALSE,
                                         create_climate_derived = TRUE,
                                         pet_monthlymean_mm_rege = 48,
                                         output_folder)



