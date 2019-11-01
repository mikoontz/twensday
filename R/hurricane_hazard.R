library(HURDAT)
library(sf)
library(magrittr)
library(dplyr)
library(googledrive)
library(raster)
library(fasterize)
library(lubridate)
library(tidyverse)

source("R/download_grid.R")

empty_grid <- download_grid() %>%
  raster


url <- "https://www.nhc.noaa.gov/gis/hazardmaps/US_SLOSH_MOM_Inundation.zip"
out_path <- file.path("data", basename(url))
download.file(url, destfile = out_path)
unzip(out_path, exdir = "data", overwrite = FALSE)

hurricane_hazard <- raster("data/US_SLOSH_MOM_Inundation_v2/US_Category4_MOM_Inundation_HighTide.tif")
plot(hurricane_hazard)

"data/US_SLOSH_MOM_Inundation_v2/US_Category4_MOM_Inundation_HighTide.tif"

system("rm data/stormsurge.tif")
system("gdalwarp data/US_SLOSH_MOM_Inundation_v2/US_Category4_MOM_Inundation_HighTide.tif data/stormsurge.tif -t_srs '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0' -tr 250 250")

storm_surge <- raster('data/stormsurge.tif')

