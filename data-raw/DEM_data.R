## code to prepare `DEM_data` dataset goes here
library(dplyr)
library(terra)

# "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
x <- rast("data-raw/DGM/DGM_model_region.tif")
x

y <- rast("data-raw/LC_corine/example_grid.asc")
crs(y) <- "EPSG:25832"
y

# z<- terra::vect("data-raw/LC_corine/LC_model_region.shp")
# z
DEM_data <- terra::resample(x, y, method = "bilinear")

rm(x, y)
plot(DEM_data)
DEM_data <- terra::as.data.frame(DEM_data, xy = TRUE)
names(DEM_data) <- c("x", "y", "DEM")

# sysdata_filenames <- load("R/sysdata.rda")
# save(
#   list = c(sysdata_filenames, "DEM_data"),
#   file = "R/sysdata.rda",
#   compress = "xz"
# )

#
usethis::use_data(DEM_data, overwrite = TRUE)
# usethis::use_data(DEM_data, overwrite = TRUE, internal = TRUE)
