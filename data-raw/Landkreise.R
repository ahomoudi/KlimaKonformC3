## code to prepare `Landkreise` dataset goes here
library(sf)

Landkreise <- sf::st_read("data-raw/Landkreise/Landkreise.shp") %>%
  dplyr::select("GEN", "geometry") %>%
  sf::st_as_sf()

sf::st_crs(Landkreise) <- NA
names(Landkreise) <- c("Name", "geometry")

usethis::use_data(Landkreise, overwrite = TRUE)
usethis::use_data(Landkreise, overwrite = TRUE, internal = TRUE)

#+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
