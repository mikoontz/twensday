library(raster)
library(tidyverse)

tornadoes <- raster('output/tornado_hazard.tif')
hurricanes <- raster('output/hurricane_hazard.tif')

tq <- quantile(tornadoes, .9)
plot(tornadoes > quantile(tornadoes, .9))
