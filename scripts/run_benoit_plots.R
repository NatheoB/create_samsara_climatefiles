#####################################################
#####################################################

# Create climate files for Benoit's plots in France

# Script from Natheo Beauchamp
# beauchamp.natheo@gmail.com
# 16/12/2023
                                                   
#####################################################
#####################################################

# Import libraries
library(terra)
library(dplyr)
library(purrr)
library(lubridate)
library(readr)
library(httr)

# Source functions in R folder
message_source_files <- sapply(grep("R$", list.files("R", recursive = TRUE), value = TRUE), 
                               function(x) source(file.path("R", x)))

# Coordinates of the sites
coords <- data.frame(
  id = c("prenovel", "saillat", "vivey", "col_porte", "col_epine", "pesse", "vaujany"),
  longitude = c(5.82765, 0.846398, 5.075669, 5.765170, 5.823752, 5.860935, 6.095338),
  latitude = c(46.52666, 45.871686, 47.735147, 45.290206, 45.579906, 46.284868, 45.201113),
  year_min = rep(2018, times = 7),
  year_max = rep(2018, times = 7),
  altitude = c(NA, NA, NA, NA, NA, NA, NA),
  rooting_depth_m = c(NA, NA, NA, NA, NA, NA, NA),
  swhc_mm = c(NA, NA, NA, NA, NA, NA, NA)
)

# Write climate files for each site
# And also compute derived climate variables (sgdd and aet2pet)
fps_files <- create_samsarafiles_climate(coords, 
                                         output_folder = "output/benoit",
                                         derived_vars = TRUE)

# Create output report to compare plots 


