## code to prepare `standard_output_de` dataset goes here
standard_output_de <- read.csv("data-raw/standard_output_de.csv",
  stringsAsFactors = FALSE
)[, 1:7]

sysdata_filenames <- load("R/sysdata.rda")
save(list = c(sysdata_filenames, "standard_output_de"),
     file = "R/sysdata.rda",
     compress='xz')


# usethis::use_data(standard_output_de, overwrite = TRUE)
# usethis::use_data(standard_output_de, overwrite = TRUE, internal = TRUE)

