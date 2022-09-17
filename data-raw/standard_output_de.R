## code to prepare `standard_output_de` dataset goes here
standard_output_de <- read.csv("data-raw/standard_output_de.csv",
                               stringsAsFactors = FALSE)[,1:7]

usethis::use_data(standard_output_de, overwrite = TRUE)
usethis::use_data(standard_output_de, overwrite = TRUE, internal = TRUE)
