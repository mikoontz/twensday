#Plot multihazard and zillow structure

library(tidvyerse)
library(raster)
library(data.table)

zillow <- raster::raster("data/V3_all_counts/temp_slice_pts_temp_inf_area_counts_2015.tif")
earthquake <- raster::raster("output/hazards/earthquake_zillow-grid.tif")
fire <- raster::raster("output/hazards/fire_zillow-grid.tif")
flood <- raster::raster("output/hazards/flood-defended_zillow-grid.tif")
hurricane <- raster::raster("output/hazards/hurricane-wind_zillow-grid.tif")
tornado <- raster::raster("output/hazards/tornado_zillow-grid.tif")

hazards <-
  stack(earthquake, fire, flood, hurricane, tornado)
# Build a matrix of the values from the raster layers

multihazard_matrix <- getValues(hazards)

# Convert to a data.table for fast manipulation
multihazard_dt <- as.data.table(multihazard_matrix)

# Assign a unique cell ID to each row 
multihazard_dt[, cellID := 1:.N]

# create a data.table that has no NAs
complete_cases_idx <- which(complete.cases(multihazard_dt))
no_na_hazmat <- multihazard_dt[complete_cases_idx, ]

# # write raw values to disk
# data.table::fwrite(x = no_na_hazmat, file = file.path("output", "multihazard-raw_conus.csv"))
# 

# Calculate the marginal CDFs for each hazard using the fast ranking technique
multihazard_dt[, `:=`(earthquake_prob = frankv(earthquake, na.last = "keep", ties.method = "max") / length(which(!is.na(earthquake))),
                      fire_prob = frankv(fire, na.last = "keep", ties.method = "max") / length(which(!is.na(fire))),
                      flood_prob = frankv(flood, na.last = "keep", ties.method = "max") / length(which(!is.na(flood))),
                      hurricane_prob = frankv(hurricane, na.last = "keep", ties.method = "max") / length(which(!is.na(hurricane))),
                      tornado_prob = frankv(tornado, na.last = "keep", ties.method = "max") / length(which(!is.na(tornado))))]


# Create individual rasters of the marginal Pr(X <= x) values for each hazard
earthquake <- zillow
values(earthquake) <- multihazard_dt$earthquake_prob

fire <- zillow
values(fire) <- multihazard_dt$fire_prob

flood <- zillow
values(flood) <- multihazard_dt$flood_prob

hurricane <- zillow
values(hurricane) <- multihazard_dt$hurricane_prob

tornado <- zillow
values(tornado) <- multihazard_dt$tornado_prob


# Set up the plots for arranging in a multi-panel plot
p_earthquake <- ~plot(earthquake, col = colorspace::sequential_hcl(n = 100, palette = "BrwnYl"), main = "Relative earthquake hazard")
p_fire <- ~plot(fire, col = viridis(100, option = "inferno"), main = "Relative fire hazard")
p_flood <- ~plot(flood, col = colorspace::sequential_hcl(n = 100, palette = "Teal"), main = "Relative flood hazard")
p_tornado <- ~plot(tornado, col = viridis(100, option = "E"), main = "Relative tornado hazard")
p_hurricane <- ~plot(hurricane, col = colorspace::sequential_hcl(n = 100, palette = "Purple-Blue"), main = "Relative hurricane wind hazard")
p_hurricane <- ~plot(zillow, col = colorspace::sequential_hcl(n = 100, palette = "Purple-Blue"), main = "Relative hurricane wind hazard")

hazard_plots <- plot_grid(p_earthquake, p_fire, p_flood,
                          p_tornado, p_hurricane, p_hurricane, p_zillow, nrow = 2, ncol = 3)
