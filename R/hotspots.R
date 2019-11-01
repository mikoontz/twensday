library(raster)
library(tidyverse)
library(mapview)
library(viridis)
library(sf)


# Tornado hotspots --------------------------------------------------------

tornadoes <- raster('output/smoothed_tornado_hazard.tif')

conus <- st_read("https://github.com/PublicaMundi/MappingAPI/raw/master/data/geojson/us-states.json") %>%
  filter(!name %in% c("Alaska", "Hawaii", "Puerto Rico")) %>%
  st_transform(projection(tornadoes))

plot(conus)

torn_masked <- mask(tornadoes, conus)

nonzero_mask <- torn_masked > 0
cellStats(nonzero_mask, mean) # fraction of nonzero cells
nz_torn_masked <- mask(torn_masked, nonzero_mask, maskvalue = 0)


tq <- quantile(torn_masked, .9)
nz_tq <- quantile(nz_torn_masked, .9)

tmask <- torn_masked > tq
nz_tmask <- torn_masked > nz_tq


tornado_hotspots <- mask(tmask, tmask, maskvalue = 0)
nz_tornado_hotspots <- mask(nz_tmask, nz_tmask, maskvalue = 0)



# Hurricane storm surge hotspots ------------------------------------------

storm_surge <- raster("data/stormsurge.tif")
surge_masked <- mask(storm_surge, conus)

ssz <- quantile(surge_masked, .9)

ss_mask <- surge_masked > ssz

ss_hotspots <- mask(ss_mask, ss_mask, maskvalue = 0)



# Visualize ---------------------------------------------------------------

mapviewOptions(na.color = NA)

# Tornadoes
# hotspots (including zeros in quantile calculations)
mapview(tornado_hotspots, col.regions = "orange", alpha.regions = 0.7)

# hotspots (excluding zeros)
mapview(nz_tornado_hotspots, col.regions = "red", alpha.regions = 0.7)

# Hurricanes
mapview(ss_hotspots, col.regions = "dodgerblue", alpha.regions = 0.7)


# Tornadoes and hurricanes
mapview(nz_tornado_hotspots, col.regions = "red", alpha.regions = 0.7) + 
  mapview(ss_hotspots, col.regions = "dodgerblue", alpha.regions = 0.7)



# Write output files ------------------------------------------------------

writeRaster(nz_tornado_hotspots, 'output/tornado_hotspots.tif')
writeRaster(ss_hotspots, 'output/stormsurge_hotspots.tif')
