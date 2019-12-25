# Resample the Fire risk map to be at the same resolution/extent as the Zillow data

library(tidyverse)
library(raster)
library(googledrive)

# make the functions available to download the Zillow grid
source("R/download_grid.R")

# Ensure the directory structure for data output is present
dir.create("output/hazards", recursive = TRUE)

# Get the empty template grid of the Zillow dataset
empty_grid <- 
  download_grid() %>%
  raster()

# These set up the variables to be used to get the hazard data and name
# new output files appropriately
hazard_name <- "hurricane-wind"
hazard_file <- "CycloneFrequency_1980_2000_projected/gdcyc_NAD.tif"
zip_path <- file.path("data", "hazards", "CycloneFrequency_1980_2000_projected.zip")

# The hurricane wind data is on the Google Drive
hazard_id <- "1REzIWNeq4zwwZdiTT2YBa7UYXTYA-r2s"

# Names of the files (to read and manipulate, and then what to call it upon
# export)
hazard_path_src <- file.path("data", "hazards", hazard_name, hazard_file)
hazard_path_out <- file.path("output", "hazards", paste0(hazard_name, "_zillow-grid.tif"))

overwrite <- FALSE

if(!file.exists(hazard_path_out) | overwrite) {
  
  # download the raw data from Google Drive
  hazard_metadata <- googledrive::drive_get(id = hazard_id)
  googledrive::drive_download(hazard_metadata, path = zip_path)
  
  # unzip the data file
  unzip(zip_path, overwrite = FALSE, exdir = file.path("data", "hazards", hazard_name))
  unlink(zip_path)
  
  # resample the raster data to be on the Zillow grid
  hazard_orig <- raster::raster(hazard_path_src)
  hazard <- raster::resample(x = hazard_orig, y = empty_grid, method = "bilinear")
  
  # Make 0/NA handling consistent by using a 0 within CONUS for "no hazard"
  hazard[is.na(hazard[])] <- 0
  
  # Then mask out the pixels outside of CONUS using the fire hazard layer
  # (which already properly counts "no hazard" as 0 and NA as "outside of CONUS")
  mask <- raster::raster("output/hazards/fire_zillow-grid.tif")
  hazard <- raster::mask(x = hazard, mask = mask)
  
  # write to disk
  raster::writeRaster(x = hazard, filename = hazard_path_out, overwrite = overwrite)
  
}

# Alternative source?
# hazard_file <- "gdcyc/gdcyc.asc"
# hazard_id <- '1whh-JSmF7v6vJm35lgQAAt5bs01Phb_t'
# zip_path <- file.path("data", "hazards", "gdcyc_cyclone.zip")
