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

dir.create(file.path("output", "hazards"), recursive = TRUE)

hazard_name <- "hurricane-stormsurge"
hazard_file <- "US_SLOSH_MOM_Inundation_v2/US_Category4_MOM_Inundation_HighTide.tif"
url <- "https://www.nhc.noaa.gov/gis/hazardmaps/US_SLOSH_MOM_Inundation.zip"

download_hazard(url = url, 
                hazard_name = hazard_name, 
                hazard_file = hazard_file, 
                overwrite = TRUE)

hazard_path_src <- file.path("data", "hazards", hazard_name, hazard_file)
hazard_path_out <- file.path("output", "hazards", paste0(hazard_name, "_zillow-grid.tif"))

overwrite <- FALSE

if(!file.exists(hazard_path_out) | overwrite) {
  
  hazard_path_tmp <- file.path("data", "hazards", hazard_name, paste0(hazard_name, "_temp.tif"))
  
  gdalwarp(srcfile = hazard_path_src, 
           dstfile = hazard_path_tmp, 
           t_srs = crs(empty_grid), 
           tr = c(250, 250), 
           overwrite = overwrite)
  
  gdalUtils::align_rasters(unaligned = hazard_path_tmp, 
                           reference = empty_grid@file@name, 
                           dstfile = hazard_path_out, 
                           overwrite = overwrite)
  
  unlink(hazard_path_tmp)
  
  # Read the hazard layer using the raster package so we can mask it
  hazard <- raster::raster(hazard_path_out)
  
  # Make 0/NA handling consistent by using a 0 within CONUS for "no hazard"
  hazard[is.na(hazard[])] <- 0
  
  # Then mask out the pixels outside of CONUS using the fire hazard layer
  # (which already properly counts "no hazard" as 0 and NA as "outside of CONUS")
  mask <- raster::raster("output/hazards/fire_zillow-grid.tif")
  hazard <- raster::mask(x = hazard, mask = mask)
  
  # write to disk
  raster::writeRaster(x = hazard, filename = hazard_path_out, overwrite = overwrite)
  
}

