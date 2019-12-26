# Purpose: Implement a multidimensional empirical cumulative distribution function
# as a means to map the multihazard values at each pixel in CONUS to a [0, 1] 
# interval representing the probability of a set of multihazard values being 
# less than or equal to the set at each pixel.

library(tidyverse)
library(raster)
library(tdigest)
library(data.table)
library(Emcdf)
library(USAboundaries)
library(sf)

# test case for Florida

fl <- 
  USAboundaries::us_states(states = "Florida", resolution = "high") %>% 
  sf::st_transform(crs = crs(hazards))

plot(st_geometry(fl))

hazard_list <- list.files("output/hazards_florida/", full.names = TRUE)
hazards <- lapply(hazard_list, raster::raster) %>% raster::stack()

multihazard_matrix <- getValues(hazards)

obj <- initF(multihazard_matrix, 4)

(start <- Sys.time())
hazard_mecdf <- emcdf(obj, multihazard_matrix[1:1e5, ])
(Sys.time() - start)