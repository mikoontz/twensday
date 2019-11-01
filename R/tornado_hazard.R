library(googledrive)
# remotes::install_github('ropensci/rnoaa')
library(rnoaa)
library(raster)
library(fasterize)
library(sf)
library(magrittr)
library(tidyverse)

source("R/download_grid.R")

empty_grid <- download_grid() %>%
  raster

conus <- st_read("https://github.com/PublicaMundi/MappingAPI/raw/master/data/geojson/us-states.json") %>%
  filter(!name %in% c("Alaska", "Hawaii", "Puerto Rico")) %>%
  st_transform(projection(empty_grid))

# Get the tornado data
tornado_paths <- tornadoes() %>%
  as('sf') %>%
  st_transform(crs = projection(empty_grid))

# subset to a particular time interval
# 1982 is when F-scale rating were consistently recorded
max_year <- 2018
min_year <- 1982
n_year <- length(min_year:max_year)
tornado_paths <- tornado_paths %>%
  filter(yr >= min_year, 
         yr <= max_year)

# buffer at 300 m
# https://journals.ametsoc.org/doi/pdf/10.1175/JAMC-D-13-0235.1
buffered_paths <- st_buffer(tornado_paths, dist = 300)

# Compute an annual empirical frequency for each grid cell
# (# years with events / total # years)
# and save to a tif file
tornado_counts <- buffered_paths %>%
  group_by(yr) %>%
  summarize %>% 
  st_cast("MULTIPOLYGON") %>%
  fasterize(raster = empty_grid, fun = 'sum', background = 0) %>%
  mask(conus)
tornado_freq <- tornado_counts / n_year
plot(tornado_freq)

# Smoothing
gf <- focalWeight(tornado_freq, 10000, "circle")
rg <- focal(tornado_freq, w = gf) %>%
  mask(conus)
plot(rg, col = viridis::viridis(100))

rg_01 <- rg / cellStats(rg, stat = "sum")
rg_scaled <- rg_01 * cellStats(tornado_freq, stat = "sum")

cellStats(tornado_freq, stat = "sum")
cellStats(rg_scaled, stat = "sum")

writeRaster(tornado_freq, 
            file.path('output', 'tornado_hazard.tif'), 
            overwrite = TRUE)
writeRaster(rg_scaled, 
            file.path('output', 'smoothed_tornado_hazard.tif'), 
            overwrite = TRUE)
