# Purpose: Implement a multidimensional empirical cumulative distribution function
# as a means to map the multihazard values at each pixel in CONUS to a [0, 1] 
# interval representing the probability of a set of multihazard values being 
# less than or equal to the set at each pixel.

library(tidyverse)
library(raster)
library(tdigest)
library(data.table)
library(Emcdf)

fire <- raster::raster("data/fired_hazard.tif")
# stormsurge <- raster::raster("data/stormsurge.tif") # different extent
flood <- raster::raster("data/FloodPlains_250m.tif")
tornado <- raster::raster("data/smoothed_tornado_hazard.tif")

compareRaster(fire, flood, tornado)
res(fire)
res(flood)
res(tornado)

multihazard <- raster::brick(fire, flood, tornado)
multihazard_matrix <- getValues(multihazard)
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