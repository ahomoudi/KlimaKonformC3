unlink(".RData")
args_in <- readLines("input_text_public_output")

netCDF.file <- args_in[1]
variable <- args_in[2]
region <- args_in[3]
landcover <- args_in[4]
language <- args_in[5]
stat_var <- args_in[6]
run_id <- args_in[7]
output_path <- args_in[8]
output_csv <- args_in[9]


library(KlimaKonformC3)




# if(!file.exists(plot_name) ){#| file.size(plot_name)==0){
ens_public_output(
  netCDF.file,
  variable,
  region,
  landcover,
  stat_var,
  language,
  run_id,
  output_path,
  output_csv
)
# }
