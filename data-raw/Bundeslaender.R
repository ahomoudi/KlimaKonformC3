## code to prepare `Bundeslaender` dataset goes here
library(sf)

Bundeslaender <- sf::st_read("data-raw/Bundeslaender/Bundeslaender.shp") %>%
  dplyr::select("GEN", "geometry") %>%
  sf::st_as_sf()

sf::st_crs(Bundeslaender) <- NA
names(Bundeslaender) <- c("Name", "geometry")
Bundeslaender$Name <- c("Bayern", "Sachsen", "Sachsen-Anhalt", "Thueringen")

sysdata_filenames <- load("R/sysdata.rda")
save(list = c(sysdata_filenames, "Bundeslaender"),
     file = "R/sysdata.rda",
     compress='xz')

# usethis::use_data(Bundeslaender, overwrite = TRUE)
# usethis::use_data(Bundeslaender, overwrite = TRUE, internal = TRUE)

#+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
