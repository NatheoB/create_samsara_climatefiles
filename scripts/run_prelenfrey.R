#####################################################
#####################################################

# Create climate files for Benoit's plots in France

# Script from Natheo Beauchamp
# beauchamp.natheo@gmail.com
# 16/12/2023

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
output_folder = "output/prelenfrey"

coords <- data.frame(
  id = c("prelenfrey"),
  longitude = c(5.82765),
  latitude = c(46.52666),
  year_min = 1983,
  year_max = 2018,
  altitude = c(NA),
  rooting_depth_m = c(NA),
  swhc_mm = c(NA)
)

# Write climate files for each site
# And also compute derived climate variables (sgdd and aet2pet)
fps_files <- create_samsarafiles_climate(coords, 
                                         create_weather = FALSE,
                                         create_climate_monthly = TRUE,
                                         create_climate_daily = FALSE,
                                         create_climate_derived = TRUE,
                                         pet_monthlymean_mm_rege = 48,
                                         output_folder)

# Create output reports
# fps_report <- create_climate_reports(output_folder)
