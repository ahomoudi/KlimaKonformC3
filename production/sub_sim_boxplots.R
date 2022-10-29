unlink(".RData")
args_in<-readLines("input_text_boxplots")

netCDF.file <-args_in[1]
region <- args_in[2]
landcover <- args_in[3]
#y.axis.limits<-c(args_in[4],args_in[5])
language <- args_in[4]
run_id <- args_in[5]
output_path <- args_in[6]
output_csv <- args_in[7]


library(KlimaKonformC3)

sim_boxplots(netCDF.file = netCDF.file,
                 region = region,
                 landcover = landcover,
                 y.axis.limits = NA,
                 language = language,
                 run_id = run_id,
                 output_path = output_path,
                 output_csv = output_csv)


#test
# netCDF.file = netCDF.file
# region = region
# landcover = landcover
# language = language
# run_id = run_id
# output_path = output_path
# output_csv = output_csv
