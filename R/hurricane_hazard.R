library(HURDAT)
library(sf)
library(magrittr)
library(dplyr)
library(googledrive)
library(raster)
library(fasterize)
library(lubridate)


source("R/download_grid.R")

empty_grid <- download_grid() %>%
  raster


# Load the HURDAT data
hurdat <- get_hurdat() %>%
  filter(!is.na(Lon), !is.na(Lat)) %>%
  st_as_sf(coords = c("Lon", "Lat"), 
           crs = 4326, agr = "constant") %>%
  st_transform(crs = projection(empty_grid))

# Filter to the years 1983-2018 ('83 is a breakpoint in the data)
min_year <- 1983
max_year <- 2018
n_year <- length(min_year:max_year)

hurdat <- hurdat %>%
  filter(year(DateTime) >= min_year, 
         year(DateTime) <= max_year)

plot(hurdat['Wind'])

# filter to hurricanes and buffer paths
# at 47 km (mean radius of maximum wind according to Wikipedia)
hurdat_buffered <- hurdat %>%
  group_by(Key) %>%
  filter(any(Status == "HU")) %>%
  summarize(n_pt = n(), 
            do_union = FALSE, 
            year = min(year(DateTime))) %>%
  st_cast("LINESTRING") %>% 
  st_buffer(47000) 
plot(hurdat_buffered)

# for each year, take the union, and use it to compute an empirical
# annual probability that a hurricane passes over a grid cell
annual_hurdat <- hurdat_buffered %>%
  group_by(year) %>%
  summarize %>%
  st_cast("MULTIPOLYGON")
plot(annual_hurdat)

# grid and save
hurdat_raster <- fasterize(annual_hurdat, raster = empty_grid, fun = "sum", 
                           background = 0)
hurdat_freq <- hurdat_raster / n_year
plot(hurdat_freq)
writeRaster(hurdat_freq, file.path('output', 'hurricane_hazard.tif'), 
            overwrite = TRUE)
