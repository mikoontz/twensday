# Purpose: Identify some hotspots for hazards and multihazards in CONUS

library(tidyverse)
library(data.table)
library(raster)
library(fasterize)
library(viridis)

source("R/download_grid.R")

empty_grid <-
  download_grid() %>% 
  raster::raster()

values(empty_grid) <- NA

multihazard_dt <- data.table::fread("output/multihazard-ecdf_conus.csv")

# earthquake
earthquake <- multihazard_dt$earthquake_prob

earthquake_hot_and_coldspot <- rep(NA, ncell(empty_grid))
earthquake_hot_and_coldspot[earthquake <= 0.1] <- 0
earthquake_hot_and_coldspot[earthquake >= 0.9] <- 1

earthquake_raster <- empty_grid
values(earthquake_raster) <- earthquake_hot_and_coldspot

ncell_cold <- length(which(earthquake_hot_and_coldspot == 0))
area_cold <- ncell_cold * 250*250 / 1e6
ncell_hot <- length(which(earthquake_hot_and_coldspot == 1))
area_hot <- ncell_hot * 250*250 / 1e6

plot(earthquake_raster, col = c("blue", "red"), legend = FALSE, 
     main = paste0("Earthquake hot and coldspots\nHotspot cells: ", ncell_hot, "; Hotspot area (km^2): ", area_hot, "\nColdspot cells: ", ncell_cold, "; Coldspot cells (km^2): ", area_cold))

png("figures/hot-and-coldspots_earthquake.png", width = 10, height = 10, units = "in", res = 600)
plot(earthquake_raster, col = c("blue", "red"), legend = FALSE, 
     main = paste0("Earthquake hot and coldspots\nHotspot cells: ", ncell_hot, "; Hotspot area (km^2): ", area_hot, "\nColdspot cells: ", ncell_cold, "; Coldspot cells (km^2): ", area_cold))
dev.off()

# fire
fire <- multihazard_dt$fire_prob

fire_hot_and_coldspot <- rep(NA, ncell(empty_grid))
fire_hot_and_coldspot[fire <= 0.1] <- 0
fire_hot_and_coldspot[fire >= 0.9] <- 1

fire_raster <- empty_grid
values(fire_raster) <- fire_hot_and_coldspot

ncell_cold <- length(which(fire_hot_and_coldspot == 0))
area_cold <- ncell_cold * 250*250 / 1e6
ncell_hot <- length(which(fire_hot_and_coldspot == 1))
area_hot <- ncell_hot * 250*250 / 1e6

plot(fire_raster, col = c("blue", "red"), legend = FALSE, 
     main = paste0("fire hot and coldspots\nHotspot cells: ", ncell_hot, "; Hotspot area (km^2): ", area_hot, "\nColdspot cells: ", ncell_cold, "; Coldspot cells (km^2): ", area_cold))

png("figures/hot-and-coldspots_fire.png", width = 10, height = 10, units = "in", res = 600)
plot(fire_raster, col = c("blue", "red"), legend = FALSE, 
     main = paste0("fire hot and coldspots\nHotspot cells: ", ncell_hot, "; Hotspot area (km^2): ", area_hot, "\nColdspot cells: ", ncell_cold, "; Coldspot cells (km^2): ", area_cold))
dev.off()

# flood
flood <- multihazard_dt$flood_prob

flood_hot_and_coldspot <- rep(NA, ncell(empty_grid))
flood_hot_and_coldspot[flood <= 0.1] <- 0
flood_hot_and_coldspot[flood >= 0.9] <- 1

flood_raster <- empty_grid
values(flood_raster) <- flood_hot_and_coldspot

ncell_cold <- length(which(flood_hot_and_coldspot == 0))
area_cold <- ncell_cold * 250*250 / 1e6
ncell_hot <- length(which(flood_hot_and_coldspot == 1))
area_hot <- ncell_hot * 250*250 / 1e6

plot(flood_raster, col = c("blue", "red"), legend = FALSE, 
     main = paste0("flood hot and coldspots\nHotspot cells: ", ncell_hot, "; Hotspot area (km^2): ", area_hot, "\nColdspot cells: ", ncell_cold, "; Coldspot cells (km^2): ", area_cold))

png("figures/hot-and-coldspots_flood.png", width = 10, height = 10, units = "in", res = 600)
plot(flood_raster, col = c("blue", "red"), legend = FALSE, 
     main = paste0("flood hot and coldspots\nHotspot cells: ", ncell_hot, "; Hotspot area (km^2): ", area_hot, "\nColdspot cells: ", ncell_cold, "; Coldspot cells (km^2): ", area_cold))
dev.off()

# hurricane.wind
hurricane.wind <- multihazard_dt$hurricane.wind_prob

hurricane.wind_hot_and_coldspot <- rep(NA, ncell(empty_grid))
hurricane.wind_hot_and_coldspot[hurricane.wind <= 0.1] <- 0
hurricane.wind_hot_and_coldspot[hurricane.wind >= 0.9] <- 1

hurricane.wind_raster <- empty_grid
values(hurricane.wind_raster) <- hurricane.wind_hot_and_coldspot

ncell_cold <- length(which(hurricane.wind_hot_and_coldspot == 0))
area_cold <- ncell_cold * 250*250 / 1e6
ncell_hot <- length(which(hurricane.wind_hot_and_coldspot == 1))
area_hot <- ncell_hot * 250*250 / 1e6

plot(hurricane.wind_raster, col = c("blue", "red"), legend = FALSE, 
     main = paste0("hurricane.wind hot and coldspots\nHotspot cells: ", ncell_hot, "; Hotspot area (km^2): ", area_hot, "\nColdspot cells: ", ncell_cold, "; Coldspot cells (km^2): ", area_cold))

png("figures/hot-and-coldspots_hurricane.wind.png", width = 10, height = 10, units = "in", res = 600)
plot(hurricane.wind_raster, col = c("blue", "red"), legend = FALSE, 
     main = paste0("hurricane.wind hot and coldspots\nHotspot cells: ", ncell_hot, "; Hotspot area (km^2): ", area_hot, "\nColdspot cells: ", ncell_cold, "; Coldspot cells (km^2): ", area_cold))
dev.off()

# hurricane.stormsurge
hurricane.stormsurge <- multihazard_dt$hurricane.stormsurge_prob

hurricane.stormsurge_hot_and_coldspot <- rep(NA, ncell(empty_grid))
hurricane.stormsurge_hot_and_coldspot[hurricane.stormsurge <= 0.1] <- 0
hurricane.stormsurge_hot_and_coldspot[hurricane.stormsurge >= 0.9] <- 1

hurricane.stormsurge_raster <- empty_grid
values(hurricane.stormsurge_raster) <- hurricane.stormsurge_hot_and_coldspot

ncell_cold <- length(which(hurricane.stormsurge_hot_and_coldspot == 0))
area_cold <- ncell_cold * 250*250 / 1e6
ncell_hot <- length(which(hurricane.stormsurge_hot_and_coldspot == 1))
area_hot <- ncell_hot * 250*250 / 1e6

plot(hurricane.stormsurge_raster, col = c("blue", "red"), legend = FALSE, 
     main = paste0("hurricane.stormsurge hot and coldspots\nHotspot cells: ", ncell_hot, "; Hotspot area (km^2): ", area_hot, "\nColdspot cells: ", ncell_cold, "; Coldspot cells (km^2): ", area_cold))

png("figures/hot-and-coldspots_hurricane.stormsurge.png", width = 10, height = 10, units = "in", res = 600)
plot(hurricane.stormsurge_raster, col = c("blue", "red"), legend = FALSE, 
     main = paste0("hurricane.stormsurge hot and coldspots\nHotspot cells: ", ncell_hot, "; Hotspot area (km^2): ", area_hot, "\nColdspot cells: ", ncell_cold, "; Coldspot cells (km^2): ", area_cold))
dev.off()

# tornado
tornado <- multihazard_dt$tornado_prob

tornado_hot_and_coldspot <- rep(NA, ncell(empty_grid))
tornado_hot_and_coldspot[tornado <= 0.1] <- 0
tornado_hot_and_coldspot[tornado >= 0.9] <- 1

tornado_raster <- empty_grid
values(tornado_raster) <- tornado_hot_and_coldspot

ncell_cold <- length(which(tornado_hot_and_coldspot == 0))
area_cold <- ncell_cold * 250*250 / 1e6
ncell_hot <- length(which(tornado_hot_and_coldspot == 1))
area_hot <- ncell_hot * 250*250 / 1e6

plot(tornado_raster, col = c("blue", "red"), legend = FALSE, 
     main = paste0("tornado hot and coldspots\nHotspot cells: ", ncell_hot, "; Hotspot area (km^2): ", area_hot, "\nColdspot cells: ", ncell_cold, "; Coldspot cells (km^2): ", area_cold))

png("figures/hot-and-coldspots_tornado.png", width = 10, height = 10, units = "in", res = 600)
plot(tornado_raster, col = c("blue", "red"), legend = FALSE, 
     main = paste0("tornado hot and coldspots\nHotspot cells: ", ncell_hot, "; Hotspot area (km^2): ", area_hot, "\nColdspot cells: ", ncell_cold, "; Coldspot cells (km^2): ", area_cold))
dev.off()