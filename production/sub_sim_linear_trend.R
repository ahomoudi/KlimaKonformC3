unlink(".RData")
args_in<-readLines("input_text_linear_trend")

netCDF.file <-args_in[1]
region <- args_in[2]
landcover <- args_in[3]
#y.axis.limits<-c(args_in[4],args_in[5])
language <- args_in[4]
run_id <- args_in[5]
output_path <- args_in[6]
output_csv <- args_in[7]


library(KlimaKonformC3)

sim_linear_trend(netCDF.file,
                 variable,
                 region,
                 landcover,
                 language,
                 run_id,
                 output_path,
                 output_csv)

