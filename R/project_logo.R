project_logo <- function() {
  grid::rasterGrob(magick::image_read_svg(system.file("logos/KlimaKonform_Logo_lang_farbig.svg",
    package = "KlimaKonformC3"
  ),
  width = 240,
  height = 125
  ),
  interpolate = TRUE
  )
}

#KlimaKonform_Logo_lang_farbig.svg SVG 240x125 240x125+0+0 16-bit sRGB 36955B 0.000u 0:14.789

