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

# buffer at 10 km
buffered_paths <- st_buffer(tornado_paths, dist = 10000)

# Compute an annual empirical frequency for each grid cell
# (# years with events / total # years)
# and save to a tif file
tornado_counts <- buffered_paths %>%
  group_by(yr) %>%
  summarize %>% 
  st_cast("MULTIPOLYGON") %>%
  fasterize(raster = empty_grid, fun = 'sum', background = 0)
tornado_freq <- tornado_counts / n_year
plot(tornado_freq)
writeRaster(tornado_freq, 
            file.path('output', 'tornado_hazard.tif'), 
            overwrite = TRUE)
