## code to prepare `land_cover` dataset goes here
library(sf)
library(dplyr)
library(terra)
x1 <- st_read("data-raw/LC_corine/LC_model_region.shp") %>%
  select("OBJECTID", "Code_18", "geometry") %>%
  vect()

x2 <- rast("data-raw/LC_corine/example_grid.asc")
# Polygons
values(x2) <- NA
land_cover <- rasterize(x1, x2, "Code_18")
# plot(land_cover)
# terra::polys(x1)

land_cover <- terra::as.data.frame(land_cover, xy = TRUE)

# crs(land_cover) <- NA
rm(x1, x2)
gc()
sysdata_filenames <- load("R/sysdata.rda")
save(
  list = c(sysdata_filenames, "land_cover"),
  file = "R/sysdata.rda",
  compress = "xz"
)
usethis::use_data(land_cover, overwrite = TRUE)
usethis::use_data(land_cover, overwrite = TRUE, internal = TRUE)
