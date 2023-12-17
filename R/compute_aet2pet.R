#' Compute the annual actual to potential evapotranspiration for each site and each year 
#' 
#' @param data_climate A list of dataframe for each site_id containing the value 
#'  of climate variables (precipitations pr, potential evapotranspiration pet_penmam and
#'  the corrected mean temperature tascorrect) for each year and each month
#'  (output of `get_monthly_climate()` function)
#' @param data_swhc Dataset with soil water holding capacity swhc in mm (`swhc_mm` column)
#'  for each site_id (`id` column) (output of `get_swhc()` function)
#' @param data_altitude Dataset with altitudes in meters (`altitude` column)
#'  for each site_id (`id` column)(output of `get_altitude()` function)
#' 
#' @return A list of dataframe for each site id containing the value of aet2pet for each year 
#' 
compute_aet2pet <- function(data_climate, data_swhc, data_altitude) {
  
  print("Computing aet2pet...")
  
  pb <- txtProgressBar(min = 0, max = length(data_climate), style = 3)
  i <- 0
  
  # Compute sgdd for each site
  out_aet2pet <- setNames(vector("list", length(data_climate)), names(data_climate))
  for (site in names(data_climate)) {
  
    # Get info of the plot
    swhc <- data_swhc$swhc_mm[data_swhc$id == site]
    altitude <- data_altitude$altitude[data_altitude$id == site]
    
    # For all years excepted the two first (used for burning years of the first target year)
    years <- sort(unique(data_climate[[site]]$year))[-c(1:2)]
    
    out_aet2pet_site <- setNames(vector("list", length(years)), years)
    
    for (y_target in years) {
      
      # Initialize the soil water content at soil water holding capacity
      # And the snow storage at 0
      swc <- swhc
      snow_storage <- 0
      
      # For year-2 to year (2 years of burning and the interest year)
      out_aet2pet_months <- rep(NA, 12)
      for (y in (y_target-2):y_target) {
        
        # For all months
        for (m in 1:12) {
          
          # Get climatic variables
          pr <- data_climate[[site]] %>% 
            dplyr::filter(month == m, year == y) %>% 
            dplyr::pull(pr)
          
          pet <- data_climate[[site]] %>% 
            dplyr::filter(month == m, year == y) %>% 
            dplyr::pull(pet_penman)
          
          tas <- data_climate[[site]] %>% 
            dplyr::filter(month == m, year == y) %>% 
            dplyr::pull(tascorrect)
          
          # Compute the amount of both snow and liquid water (From McCabe and Markstrom)
          out_water <- compute_water_McCandM(tas, pr, snow_storage, altitude)
          snow_storage <- out_water$snow_storage
          
          # Update the soil water content as Piedallu method but with new liquid water from snow and precipitations
          out_wb <- compute_waterbalance_Piedallu(swc, swhc, out_water$lw, pet)
          swc <- out_wb$swc
          
          # Store aet2pet of the month if year of interest
          if (y == y_target) {
            out_aet2pet_months[m] <- out_wb$aet2pet
          }
          
        }
      
      }
      out_aet2pet_site[[as.character(y_target)]] <- data.frame(
        year = y_target,
        aet2pet = mean(out_aet2pet_months)
      )
      
    }
    out_aet2pet[[site]] <- dplyr::bind_rows(out_aet2pet_site)
  
    # Set progress bar
    i <- i+1
    setTxtProgressBar(pb, value = i)
  }
  close(pb)
  
  return(out_aet2pet)
}


#' Compute the proportion of liquid water of the month and update the snow reservoire
#' 
#' @param tas Monthly mean temperature of the air surface (in °C)
#' @param pr Monthly precipitations on all the forms (in mm)
#' @param snow_storage Amount of snow water equivalent in the reservoir (in mm)
#' @param altitude Altitude of the plot (in meters)
#' 
#' @return Return a list with :
#' \itemize{
#'  \item{snowfrac}{Fraction of the total precipitations that are solid (snow)}
#'  \item{snowmeltfrac}{Fraction of the snow from the snow reservoire that melt into liquid water}
#'  \item{pr.snow}{Snow precipitations in mm (snow water equivalent)}
#'  \item{pr.liquid}{Liquid precipitations in mm}
#'  \item{mw}{Water from melted snow in mm}
#'  \item{lw}{Liquid water (from liquid precipitation and water from melted snow)}
#'  \item{snow_storage}{Updated snow water equivalent in the snow reservoir in mm}
#' }
#' 
#' @source From McCabe and Markstrom (https://pubs.usgs.gov/of/2007/1088/pdf/of07-1088_508.pdf)
#' 
compute_water_McCandM <- function(tas, pr, snow_storage, altitude) {
  
  # Computation based on 
  ##### FIXED PARAMETERS #####
  
  # Temperatures when precipitations are full rain (T_rain)
  # or full snow over or below 1000m altitude (T_snow)
  T_rain <- 3.3
  T_snow_inf1000 <- -10
  T_snow_sup1000 <- -1
  
  # Maximum snow melt fraction in a month
  snowmelt_frac_max <- 0.5
  
  
  ##### COMPUTE WATER BALANCE #####
  
  ### Compute snow accumulation 
  # (estimation of the amount of monthly precipitation (pr) that is rain (pr_rain) or snow (pr_snow), in millimeters)
  
  # Temperature above which all precipitations are snow, according to altitude
  T_snow <- ifelse(altitude > 1000, T_snow_sup1000, T_snow_inf1000)
  
  
  # Compute fraction of precipitations that will be snow
  # Linear function between Tsnow and Train, and then 0 if T is greater than T_rain or 1 if T is lower than T_snow
  # Fraction for snow that will melt and become available water is 1 - fraction of snow precipitations
  snowfrac <- pmax(0, pmin(1, (T_rain - tas) / (T_rain - T_snow)))
  
  # Compute snow precipitations as a fraction of total precipitations
  pr_snow <- snowfrac * pr
  pr_liquid <- pr - pr_snow
  
  # Compute snow that will melt from the snow reservoir into liquid available water
  # Cannot melt more than "snowmelt_frac_max" proportion of the snow reservoire
  snowmelt_frac <- pmin( (1-snowfrac), snowmelt_frac_max)
  melted_water <- snowmelt_frac * snow_storage
  
  
  # Add snow precipitations and remove snow melting from an infinite reservoir of snow water equivalent precipitaions
  snow_storage <- snow_storage + pr_snow - melted_water
  
  
  # Total liquid water are the total precipitations - snow precipitations + water from snow melting 
  liquid_water <- pr - pr_snow + melted_water
  
  
  # Return variables
  out_month <- data.frame(
    snowfrac = snowfrac, snowmeltfrac = snowmelt_frac,
    pr.snow = pr_snow, pr.liquid = pr_liquid, mw = melted_water,
    lw = liquid_water, snow_storage = snow_storage
  )
  
  return(out_month)
}


#' Compute the monthly soil water balance of the plot
#' 
#' @param swc Soil water content in mm
#' @param swhc Soil water holding capacity of the plot in mm
#' @param lw Liquid water (from liquid precipitations and snow melting) in mm
#' @param pet Potential evapotranspiration in mm
#' 
#' @return A list with :
#' \itemize{
#'  \item{swc}{Soil water content (in mm)}
#'  \item{swci}{Soil water content index (swc relative to swhc)}
#'  \item{aet2pet}{Ratio of actual to potential evapotranspirarion}
#'  \item{swd}{Soil water deficit in mm}
#' }
#' 
#' @source # Soil water balance performs better than climatic water variables in tree species distribution modelling
#'   https://doi.org/10.1111/geb.12012 (Piedallu et al. 2012)
#'
compute_waterbalance_Piedallu <- function(swc, swhc, lw, pet) {
  
  ### Compute the soil water content
  water_balance <- lw - pet
  
  # If total water added pr_tot is greater than pet
  # ==> all water needed is taken from water entry and water in excess go to soil water content
  
  # When during the month, there are less precipitations than the potential evapotranspiration
  # The lack of water is taken from the swc as a function of water deficiency and soil water holding capacity
  # The more water is lacking, the more swc is reduced
  swc_new <- ifelse(water_balance > 0, 
                    swc + water_balance,
                    swc * exp( (lw - pet) / swhc ))
  
  ### Limit soil water content to soil water holding capacity and to 0
  # And compute soil water surplus as water in excess compared to swhc
  swc_new <- pmax(swc_new, 0)
  water_surplus <- pmax( swc_new - swhc, 0 ) 
  swc_new <- pmin(swc_new, swhc)
  
  ### Compute actual evapotranspiration
  # If more precipitations than PET thus, aet is equal to pet
  # Otherwise, AET is the precipitations + the water that has been pumped from the soil water reservoir
  aet <- ifelse(water_balance > 0,
                pet,
                pmin(pet, lw + swc - swc_new))
  
  
  ### Compute soil water indicators
  
  # Soil water deficit (difference between pet and aet)
  water_deficit <- pet - aet
  
  # Compute aet to pet ratio (be careful if pet = 0)
  aet2pet_ratio <- ifelse(pet == 0, 1, aet/pet)
  
  # Soil water content index
  swci <- swc_new / swhc
  
  
  ### Store results in a list
  out_month <- list(swc = swc_new, swci = swci,
                    aet2pet = aet2pet_ratio,
                    swd = water_deficit)
  
  return(out_month)
}

