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

# read the local hazard layers into memory, downloading first
# if necessary
# the rasterStack is called "hazards"
source("R/download_configured-hazards.R")

fl <- 
  USAboundaries::us_states(states = "Florida", resolution = "high") %>% 
  sf::st_transform(crs = crs(hazards))

plot(st_geometry(fl))

conus <- st_read("https://github.com/PublicaMundi/MappingAPI/raw/master/data/geojson/us-states.json") %>%
  filter(name == "Florida") %>%
  st_transform(projection(empty_grid))


multihazard_matrix <- getValues(hazards)
small_mat <- multihazard_matrix[sample(1:nrow(multihazard_matrix), size = 1e5), ]

fire_ecdf <- tdigest::tdigest(vec = getValues(fire), compression = 1000)
tdigest::td_quantile_of(td = fire_ecdf, val = c(0.001, 1))


n = 10^6
set.seed(123)
x = rnorm(n)
y = rnorm(n)
z = rnorm(n)
data = cbind(x, y, z)
#The aim is to compute F(0.5,0.5,0.5) with three
#approaches and compare the performances.
#To avoid CPU noises, we repeat the computation 10 times.
#compute with R built-in function, sum()
sum_time = system.time({
  aws1 = c()
  for(i in 1:10)
    aws1[i] = sum(x <= 0.5& y <=0.5& z <=0.5)/n
})[3]

#compute with emcdf single-thread
a = matrix(rep(c(0.5, 0.5, 0.5), 10), 10, 3)
single_time = system.time({
  aws2 = emcdf(data, a)
})[3]
?initF

obj <- initF(small_mat, 4)

aws3 = emcdf(obj, small_mat)

aws2 == aws1
aws3 == aws1
sum_time
single_time
multi_time