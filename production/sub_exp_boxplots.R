unlink(".RData")
args_in <- readLines("input_text_boxplots")

netCDF.files <- args_in[1:4]
variable <- args_in[5]
region <- args_in[6]
landcover <- args_in[7]
y.axis.limits<-NA
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
