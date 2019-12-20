# Resample the Fire risk map to be at the same resolution/extent as the Zillow data

library(tidyverse)
library(raster)
library(rasterVis)
library(stars)
library(gdalUtils)

source("R/download_grid.R")


grid_path <- file.path("data", "EmptyGrid", 'Empty_250_US.tif')
if (!file.exists(grid_path) | overwrite) {
  spatial_grid_id <- '15bEuENdzVfKCc57DPQx2s1JSr3awqT14'
  grid_metadata <- drive_get(id = spatial_grid_id)
  zip_path <- file.path("data", "grid.zip")
  drive_download(grid_metadata, path = zip_path, overwrite = overwrite)
  unzip(zip_path, overwrite = overwrite, exdir = "data")
  unlink(zip_path)
}
stopifnot(file.exists(grid_path))
grid_path


# download the hazard raster layer and reproject to the template grid
# needs source url, hazard name
download_hazard <- function(url, hazard_name, hazard_file, empty_grid, overwrite = FALSE) {
  
  hazard_path <- file.path("output", paste0(hazard_name, "_zillow-grid.tif"))
  
  if(!file.exists(hazard_path) | overwrite) {
    
    zip_path <- file.path("data", basename(url))
    download.file(url, destfile = zip_path)
    unzip(zip_path, exdir = file.path("data", hazard_name), overwrite = FALSE)
    unlink(zip_path)

    hazard_raster <- raster(file.path("data", hazard_name, hazard_file))
    
    
    gdalwarp(srcfile = file.path("data", hazard_name, hazard_file), dstfile = hazard_path, t_srs = crs(empty_grid), tr = c(250, 250))
    
    
    
    hazard_extended <- raster::projectRaster(from = hazard_raster, to = empty_grid, method = "bilinear")
    hazard_resampled <- raster::resample(x = hazard_raster, y = empty_grid, method = "bilinear")
    
    dir.create(file.path("output", hazard_name), recursive = TRUE)
    raster::writeRaster(x = hazard_resampled, filename = hazard_path)
  }
  
}

empty_grid <- 
  download_grid() %>%
  raster()

url <- "https://www.nhc.noaa.gov/gis/hazardmaps/US_SLOSH_MOM_Inundation.zip"
hazard_name <- "stormsurge"
hazard_file <- "US_SLOSH_MOM_Inundation_v2/US_Category4_MOM_Inundation_HighTide.tif"

download_hazard(url = url, hazard_name = hazard_name, hazard_file = hazard_file, empty_grid = empty_grid)

out_path <- file.path("data", basename(url))
download.file(url, destfile = out_path)
unzip(out_path, exdir = file.path("data", hazard_name), overwrite = FALSE)

hazard_raster <- raster(file.path("data", hazard_name, hazard_file))
plot(hazard_raster)

template <- raster::raster("data/EmptyGrid/Empty_250_US.tif")

# Source: https://www.fs.usda.gov/rds/archive/catalog/RDS-2016-0034
fire <- raster::raster("data/bp_file_NationalFSim/bp_201608301.tif")
fire_resamp <- raster::resample(x = fire, y = template, method = "bilinear")
raster::writeRaster(x = fire_resamp, filename = "output/hazards-on-zillow-grid/fire-hazard_fsim-burn-probability_zillow-grid.tif")

# Source: ?
earthquake <- raster::raster("output/2014_pga2pct50yrs_reproj_cont_zero_90th_perc_annual.tif")
earthquake_resamp <- raster::resample(x = earthquake, y = template, method = "bilinear")

# Replace NA's within CONUS with 0 probability, then mask out the ocean and areas outside of CONUS
earthquake_resamp[is.na(earthquake_resamp[])] <- 0
earthquake_resamp <- mask(earthquake_resamp, fire_resamp)

raster::writeRaster(x = earthquake_resamp, filename = "output/hazards-on-zillow-grid/earthquake-hazard_zillow-grid.tif")
