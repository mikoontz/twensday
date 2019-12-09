# Purpose: Implement a multidimensional empirical cumulative distribution function
# as a means to map the multihazard values at each pixel in CONUS to a [0, 1] 
# interval representing the probability of a set of multihazard values being 
# less than or equal to the set at each pixel.
# Essentially, this tells us about the *relative* hazard risk at each pixel, compared
# to all other pixels

library(tidyverse)
library(raster)
library(data.table)
library(mltools)
library(viridis)

fire <- raster::raster("output/hazards-on-zillow-grid/fire-hazard_fsim-burn-probability_zillow-grid.tif")
tornado <- raster::raster("output/hazards-on-zillow-grid/smoothed_tornado_hazard.tif")
earthquake <- raster::raster("output/hazards-on-zillow-grid/earthquake-hazard_zillow-grid.tif")
# stormsurge <- raster::raster("data/stormsurge.tif") # different extent
# flood <- raster::raster("data/FloodPlains_250m.tif")

# Make sure the rasters can be stacked (i.e., have the same resolution/extent)
compareRaster(fire, tornado, earthquake)

# stack the rasters into a multi-band raster to ensure pixel values line up
multihazard <- raster::brick(fire, tornado, earthquake)
names(multihazard) <- c("fire", "tornado", "earthquake")

# Get the values of the hazard raster (this makes it non-spatial)
multihazard_matrix <- getValues(multihazard)
multihazard_matrix <- as_tibble(multihazard_matrix)

# convert to a data.table
dt <- data.table(multihazard_matrix)

# we need the number of non-NAs to help calculate the probabilities
# In our case, we want to completely ignore NA's because they represent
# area outside of our area of interest (rather than missing values within
# our area of interest)
non_na_idx <- complete.cases(dt)
num_non_na <- length(which(non_na_idx))

# use fast ranking and the number of non-NA values per hazard to cacluate
# the probability that each hazard value is less than or equal to all
# other values throughout CONUS of the same hazard type
dt[, `:=`(fire_prob = frankv(fire, na.last = "keep", ties.method = "max") / num_non_na,
          tornado_prob = frankv(tornado, na.last = "keep", ties.method = "max") / num_non_na,
          earthquake_prob = frankv(earthquake, na.last = "keep", ties.method = "max") / num_non_na)]

# The multivariate probability that a set of hazard values is less than or equal
# to all other sets of hazard values (in multidimensional hazard space)
dt[, multihazard_prob := fire_prob * tornado_prob * earthquake_prob]

# write to disk
fwrite(x = dt, file = "output/multihazard-rankings_mecdf_datatable.csv")

# visualize!

# Use the original fire raster as a template to take the non-spatial probability
# values and map them to the CONUS Zillow raster
multihazard_prob <- fire
multihazard_prob[] <- dt$multihazard_prob

# Use the fire raster as the spatial template for all hazards to (try to) avoid
# confusion. (instead of earthquake_prob <- earthquake, which seems more cryptic
# to me)
fire_prob <- fire
fire_prob[] <- dt$fire_prob

earthquake_prob <- fire
earthquake_prob[] <- dt$earthquake_prob

tornado_prob <- fire
tornado_prob[] <- dt$tornado_prob

# plot!
# First plot is of the multihazard probability along with the original values
# for the individual hazards

par(mfrow = c(2, 2))
plot(multihazard_prob, main = "multihazard")
plot(tornado, main = "tornado")
plot(earthquake, main = "earthquake")
plot(fire, main = "fire")

# Second plot is also of the multihazard probability but with the *relative*
# hazard values for the three hazards
png("figs/multihazard-relative-risk.png", height = 10, width = 14, res = 1000, units = "in")
par(mfrow = c(2, 2), mar = c(4, 3, 3, 3), oma = c(3, 0, 0, 0))
plot(multihazard_prob, 
     col = viridis(100, option = "D"), 
     axes = FALSE, 
     horizontal = TRUE, 
     legend.args = list(text = 'Relative multihazard risk', side = 3, line = 0.5))

plot(fire_prob, 
     col = viridis(100, option = "B"), 
     axes = FALSE, 
     horizontal = TRUE, 
     legend.args = list(text = 'Relative fire risk', side = 3, line = 0.5))

plot(tornado_prob, 
     col = viridis(100, option = "E"), 
     axes = FALSE, 
     horizontal = TRUE, 
     legend.args = list(text = 'Relative tornado risk', side = 3, line = 0.5))

plot(earthquake_prob, 
     col = viridis(100, option = "C"), 
     axes = FALSE, 
     horizontal = TRUE, 
     legend.args = list(text = 'Relative earthquake risk', side = 3, line = 0.5))

dev.off()

min(dt$multihazard_prob, na.rm = TRUE)
min_idx <- multihazard_prob
min_idx[is.na(min_idx[]) | min_idx[] > (0.1 + min(dt$multihazard_prob, na.rm = TRUE))] <- NA

par(mfrow = c(1, 1))
plot(min_idx)
