# Resample the Fire risk map to be at the same resolution/extent as the Zillow data

library(tidyverse)
library(raster)
library(rasterVis)

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
