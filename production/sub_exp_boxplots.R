#!/usr/bin/env Rscript
args_in <- commandArgs(trailingOnly = TRUE)

# test if there is at least one argument: if not, return an error
if (length(args_in) == 0) {
  stop("At least one argument must be supplied (input file).n", call. = FALSE)
}

print(args_in)

unlink(".RData")
args_in <- readLines(args_in)

netCDF.files <- args_in[1:4]
variable <- args_in[5]
region <- args_in[6]
landcover <- args_in[7]
y.axis.limits <- NA
language <- args_in[8]
run_id <- args_in[9]
output_path <- args_in[10]
output_csv <- NULL


library(KlimaKonformC3)

exp_boxplots(
  netCDF.files = netCDF.files,
  variable = variable,
  region = region,
  landcover = landcover,
  language = language,
  run_id = run_id,
  output_path = output_path,
  output_csv = output_csv
)


# test
# netCDF.file = netCDF.file
# region = region
# landcover = landcover
# language = language
# run_id = run_id
# output_path = output_path
# output_csv = output_csv
