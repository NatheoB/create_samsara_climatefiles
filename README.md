# Pipeline to create a Samsara2 site climate files

Example on how to run the pipline is shown in the file `script_example.R`

## Pipeline description

The main function to use in the function `create_samsarafiles_climate()`

### --- PLOT INFO ---

Create a file with information on each site, needed on the header of a Samsara2 inventory file

-   Extract altitude from SRTM30 database (<https://www2.jpl.nasa.gov/srtm/>)
-   Compute the soil water holding capacity (SWHC) of the site by extracting soil data from SoilGrids (<https://soilgrids.org/>) and rooting depth from the European Soil Database (<https://esdac.jrc.ec.europa.eu/content/european-soil-database-derived-data>)

### --- WEATHER ---

Create the weather file of the site for the SamsaraLight model

-   Extract radiation data from PVGIS database (<https://joint-research-centre.ec.europa.eu/photovoltaic-geographical-information-system-pvgis_en>)

### --- CLIMATE ---

Create the Samsara2 climate files for each site

-   Extract monthly climate data from Chelsa database (<https://chelsa-climate.org/>)
-   Correct the temperature data with the altitude
-   -Monthly climate- : Create the climate file with monthly raw climatic variables
-   -Daily climate- : Create the daily climate file with daily radiation and climate data used in output for Samsara2 model
-   -Derived climate- : Compute annual derived climate variables (aet2pet, sgdd and other indicators) and create the climate file with sgdd ans aet2pet (see Beauchamp et al. 2025: <https://doi.org/10.1111/1365-2745.14489>)

## Connecting to the database with input rasters

The rasters for precise altitude (SRTM30), the computed lapserate to correct the temperature with the altitude (lapserate) and the climate data (envicloud/chelsa) are stored in an INRAE database \\\\195.221.110.170\\projets\\chelsa. Thus, one needs to add a network location in Windows (I do not know for Linux users, sorry) in order to have locally access to those data from your computer. BE CAREFUL, IT IS A SHARED FOLDER WITH OTHER USERS (please do not delete the files, it is veeeeery long to re-download). See here how to do this: <https://coehelp.uoregon.edu/file-server/how-to-map-a-network-location/>. The name of the network location you give is the argument `input_folder` of the function `create_samsarafiles_climate()` .

## Input coordinates

The main input of the function `create_samsarafiles_climate()` is a data.frame with:

-   A unique id for each site (string or numeric).
-   The coordinates of each site in WGS84 (longitude/latitude)
-   The range between which to compute annual variables (minimum and maximum year) (because of the range of the Chelsa climate database, it cannot be lower than 1983 and greater than 2018)

The input data.frame thus must be composed of five columns (id, longitude, latitude, year_min, year_max).

## Output files

The function `create_samsarafiles_climate()` creates files in the folder defined in `output_folder`:

-   *samsara_plot_info.csv* : Information of the plot for Samsara2 (swhc, altitude)
-   *samsara_weather.txt* : Weather climate file used for SamsaraLight inputs (one file per plot)
-   *samsara_climate_monthly.txt* : Monthly climate file with raw climatic data from Chelsa
-   *samsara_daily_climate.txt* : Daily climate file with daily radiation and climate data used in output for Samsara2 model (one file per plot) (BE CAREFUL!!! This output is really long to write, around 5mn per site)
-   *samsara_climate_derived.txt* : Yearly sgdd and aet2pet derived variables (computed in Samsara2 model for annual growth and mortality predictions)

You can decide whether to write or not a given file with the specific argument of the function (TRUE or FALSE).

## Create a climate report

The function `create_climate_reports()` create a small climatic report for each site, with an RMarkdown report, written in the output folder of the site.
