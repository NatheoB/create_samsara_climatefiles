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

##### INPUT DATA #####

# Output folder 
input_folder <- "W://ClimateData"
output_folder <- "output/benoit" # 

# Information of the sites
coords <- data.frame(
  id = c("prelenfrey", "prenovel", "saillat", "vivey", "col_porte", "col_epine", "pesse", "vaujany"), # Unique id of the plot
  longitude = c(5.600517626, 5.82765, 0.846398, 5.075669, 5.765170, 5.823752, 5.860935, 6.095338), # WGS84 X coordinate (deg decim)
  latitude = c(45.025232032, 46.52666, 45.871686, 47.735147, 45.290206, 45.579906, 46.284868, 45.201113), # WGS84 Y coordinate (deg decim)
  year_min = rep(1983, times = 8), # Minimum year to compute climatic data (cannot be lower than 1983)
  year_max = rep(2018, times = 8), # Maximum year to compute climatic data (cannot be greater than 2018)
  altitude = rep(NA, times = 8), # NOT MANDATORY : altitude in m of the plot, if not given, it will be computed
  rooting_depth_m = rep(NA, times = 8), # NOT MANDATORY : plot rooting depth in m of the plot, if not given, it will be computed
  swhc_mm = rep(NA, times = 8) # NOT MANDATORY : soil water holding capacity in mm of the plot, if not given, it will be computed
)


##### RUN #####

# Write climate files for each site
# And also compute derived climate variables (sgdd and aet2pet)
fps_files <- create_samsarafiles_climate(coords, 
                                         create_weather = TRUE,
                                         create_climate_monthly = TRUE,
                                         create_climate_daily = TRUE,
                                         create_climate_derived = TRUE,
                                         pet_monthlymean_mm_rege = 48,
                                         input_folder,
                                         output_folder)

# Create output reports
# fps_report <- create_climate_reports(output_folder)
