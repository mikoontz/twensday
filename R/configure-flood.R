# Resample the Fire risk map to be at the same resolution/extent as the Zillow data

library(tidyverse)
library(gdalUtils)

source("R/download_grid.R")
source("R/download_hazard.R")

empty_grid <- 
  download_grid() %>%
  raster()

dir.create("output/hazards", recursive = TRUE)

hazard_name <- "flood"
hazard_file <- "Projected_Flood100yr_FathomData/flood_100_NAD.tif"
hazard_id <- '1DkyxvuQi8DQm_gGpn7GF7nnzvvz--Oyp'
zip_path <- file.path("data", "hazards", "Projected_Flood100yr_FathomData.zip")

hazard_path_src <- file.path("data", "hazards", hazard_name, hazard_file)
hazard_path_out <- file.path("output", "hazards", paste0(hazard_name, "_zillow-grid.tif"))

overwrite <- FALSE

if(!file.exists(hazard_path_out) | overwrite) {
  
  hazard_metadata <- drive_get(id = hazard_id)
  drive_download(hazard_metadata, path = zip_path)
  unzip(zip_path, overwrite = FALSE, exdir = file.path("data", "hazards", hazard_name))
  unlink(zip_path)
  
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
  
  hazard <- raster::raster(hazard_path_out)
  
  # Mask out the pixels outside of CONUS using the fire hazard layer
  # (which already properly counts "no hazard" as 0 and NA as "outside of CONUS")
  mask <- raster::raster("output/hazards/fire_zillow-grid.tif")
  hazard <- raster::mask(x = hazard, mask = mask)
  
  # write to disk
  raster::writeRaster(x = hazard, filename = hazard_path_out, overwrite = overwrite)
}