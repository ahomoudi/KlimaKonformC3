## code to prepare `standard_output_en` dataset goes here

standard_output_en <- read.csv("data-raw/standard_output_en.csv",
  stringsAsFactors = FALSE
)[, 1:7]

# save(
#   list = c("standard_output_en"),
#   file = "R/sysdata.rda",
#   compress = "xz"
# )
usethis::use_data(standard_output_en, overwrite = TRUE)
# usethis::use_data(standard_output_en, overwrite = TRUE, internal = TRUE)
