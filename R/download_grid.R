library(googledrive)

download_grid <- function(overwrite = FALSE) {
  # Download the spatial grid (if necessary) and return the path to it
  grid_path <- file.path("data", "EmptyGrid", 'Empty_250_US.tif')
  if (!file.exists(grid_path) | overwrite) {
    spatial_grid_id <- '15bEuENdzVfKCc57DPQx2s1JSr3awqT14'
    grid_metadata <- drive_get(id = spatial_grid_id)
    zip_path <- file.path("data", "grid.zip")
    drive_download(grid_metadata, path = zip_path, overwrite = overwrite)
    unzip(zip_path, overwrite = overwrite, exdir = "data")
    unlink(zip_path)
  }
  stopifnot(file.exists(grid_path))
  grid_path
}

