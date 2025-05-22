#####################################################
#####################################################

# Create climate files for plots used for fitting samsara allometries
# (Fuhr and MONTANE databases)

# Script from Natheo Beauchamp
# beauchamp.natheo@gmail.com
# 08/11/2024

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
output_folder <- "output/fuhr2017"
data_raw <- vroom("data/coords_fuhr2017.csv")

coords <- data_raw %>% 
  dplyr::select(id,
                longitude = long,
                latitude = lat,
                year = year,
                altitude = elevation) %>% 
  dplyr::mutate(
    id = as.character(id),
    year_min = year - 3,
    year_max = year,
    rooting_depth_m = NA,
    swhc_mm = NA
  )


# Compute derived climate variables (sgdd and aet2pet)
fps_files <- create_samsarafiles_climate(coords,
                                         create_weather = FALSE,
                                         create_climate_monthly = FALSE,
                                         create_climate_daily = FALSE,
                                         create_climate_derived = TRUE,
                                         pet_monthlymean_mm_rege = 48,
                                         input_folder = "S:",
                                         output_folder)



