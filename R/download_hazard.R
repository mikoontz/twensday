# download the hazard raster layer from url source
# needs source url, hazard name, the filename of the hazard (because there
# are often several files in each unzipped directory of files associated
# with a singl hazard)

download_hazard <- function(url, 
                            hazard_name, 
                            hazard_file, 
                            overwrite = FALSE) {
  
  zip_path_src <- file.path("data", "hazards", hazard_name, basename(url))
  zip_path_out <- file.path("data", "hazards", hazard_name)

  hazard_path_src <- file.path(zip_path_out, hazard_file)
  
  if(!file.exists(hazard_path_src) | overwrite) {
      dir.create(zip_path_out, recursive = TRUE)
      download.file(url, destfile = zip_path_src)
      unzip(zip_path_src, exdir = zip_path_out, overwrite = overwrite)
      unlink(zip_path_src)
    }
}