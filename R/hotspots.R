library(raster)
library(tidyverse)

tornadoes <- raster('output/tornado_hazard.tif')
hurricanes <- raster('output/hurricane_hazard.tif')

joint <- tornadoes * hurricanes

tornado_vals <- values(tornadoes)
hurricane_vals <- values(hurricanes)

idx <- sample(length(tornado_vals), 1000000)

tibble(x = tornado_vals[idx], 
       y = hurricane_vals[idx]) %>%
  ggplot(aes(x, y)) + 
  geom_count() + 
  xlab("Tornado frequency") + 
  ylab("Hurricane frequency")
