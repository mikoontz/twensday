library(raster)
library(tidyverse)
library(mapview)
library(viridis)
library(sf)

tornadoes <- raster('output/tornado_hazard.tif')
hurricanes <- raster('output/hurricane_hazard.tif')

conus <- st_read("https://github.com/PublicaMundi/MappingAPI/raw/master/data/geojson/us-states.json") %>%
  filter(!name %in% c("Alaska", "Hawaii", "Puerto Rico")) %>%
  st_transform(projection(tornadoes))

plot(conus)

torn_masked <- mask(tornadoes, conus)
hurr_masked <- mask(hurricanes, conus)

tq <- quantile(torn_masked, .95)
hq <- quantile(hurr_masked, .95)

tmask <- torn_masked > tq
hmask <- hurr_masked > hq

tornado_hotspots <- mask(tmask, tmask, maskvalue = 0)
hurricane_hotspots <- mask(hmask, hmask, maskvalue = 0)


# Visualize ---------------------------------------------------------------

mapviewOptions(na.color = NA)
mapview(tornado_hotspots, col.regions = "red", alpha.regions = 0.7) +
  mapview(hurricane_hotspots, col.regions = "dodgerblue", alpha.regions = 0.5)
