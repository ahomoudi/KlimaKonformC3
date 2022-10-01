## code to prepare `land_cover_legend` dataset goes here
land_cover_legend <- read.csv("data-raw/LC_corine/CLC_legend.csv",
  stringsAsFactors = FALSE,
  sep = ";"
)

# Function to apply
rgb2hex <- function(r, g, b) {
  rgb(r, g, b, maxColorValue = 255)
}
# Split original vector and pass to rgb2hex with do.call as list

for (i in 1:length(land_cover_legend$RGB)) {
  RGB <- unlist(strsplit(land_cover_legend$RGB[i], "-"))

  if (length(RGB) == 0) {
    land_cover_legend$Hex[i] <- NA
  } else {
    land_cover_legend$Hex[i] <- rgb2hex(
      r = RGB[1],
      g = RGB[2],
      b = RGB[3]
    )
  }
}
rm(i, RGB, rgb2hex)

sysdata_filenames <- load("R/sysdata.rda")
save(
  list = c(sysdata_filenames, "land_cover_legend"),
  file = "R/sysdata.rda",
  compress = "xz"
)

usethis::use_data(land_cover_legend, overwrite = TRUE)
usethis::use_data(land_cover_legend, overwrite = TRUE, internal = TRUE)
