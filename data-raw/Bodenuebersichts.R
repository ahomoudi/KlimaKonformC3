## code to prepare `Bodenuebersichts` dataset goes here
library(sf)
library(dplyr)
library(terra)
x1 <- st_read("data-raw/Bodenuebersichts/Bodenuebersichts.shp") %>%
  select("TKLE_NR", "geometry") %>%
  vect()

x2 <- rast("data-raw/LC_corine/example_grid.asc")
# Polygons
values(x2) <- NA
Bodenuebersichts <- rasterize(x1, x2, "TKLE_NR")
# plot(land_cover)
# terra::polys(x1)

Bodenuebersichts <- terra::as.data.frame(Bodenuebersichts, xy = TRUE)

# crs(land_cover) <- NA
rm(x1, x2)
gc()
sysdata_filenames <- load("R/sysdata.rda")
save(
  list = c(sysdata_filenames, "Bodenuebersichts"),
  file = "R/sysdata.rda",
  compress = "xz"
)
#
# usethis::use_data(Bodenuebersichts, overwrite = TRUE)
# usethis::use_data(Bodenuebersichts, overwrite = TRUE, internal = TRUE)
