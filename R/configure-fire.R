# Resample the Fire risk map to be at the same resolution/extent as the Zillow data

library(tidyverse)
library(gdalUtils)
library(viridis)

source("R/download_grid.R")
source("R/download_hazard.R")

empty_grid <- 
  download_grid() %>%
  raster()

dir.create(file.path("output", "hazards"), recursive = TRUE)

hazard_name <- "fire"
hazard_file <- "Data/whp_2018_continuous/whp2018_cnt/dblbnd.adf"
url <- "https://www.fs.usda.gov/rds/archive/products/RDS-2015-0047-2/RDS-2015-0047-2.zip"
# Source:
# Dillon, Gregory K. 2018. Wildfire Hazard Potential (WHP) for the conterminous United States (270-m GRID), version 2018 continuous. 2nd Edition. Fort Collins, CO: Forest Service Research Data Archive. https://doi.org/10.2737/RDS-2015-0047-2

download_hazard(url = url, 
                hazard_name = hazard_name, 
                hazard_file = hazard_file, 
                overwrite = TRUE)

hazard_path_src <- file.path("data", "hazards", hazard_name, hazard_file)
hazard_path_out <- file.path("output", "hazards", paste0(hazard_name, "_zillow-grid.tif"))

overwrite <- FALSE

if(!file.exists(hazard_path_out) | overwrite) {
  
  hazard_orig <- raster::raster(hazard_path_src)
  hazard <- raster::resample(x = hazard_orig, y = empty_grid, method = "bilinear")
  
  raster::writeRaster(x = hazard, filename = hazard_path_out, overwrite = overwrite)

}

# Alternative source. Just burn probability
# Short, Karen C.; Finney, Mark A.; Scott, Joe H.; Gilbertson-Day, Julie W.; Grenfell, Isaac C. 2016. Spatial dataset of probabilistic wildfire risk components for the conterminous United States. Fort Collins, CO: Forest Service Research Data Archive. https://doi.org/10.2737/RDS-2016-0034
# url <- "https://www.fs.usda.gov/rds/archive/products/RDS-2016-0034/RDS-2016-0034.zip"
