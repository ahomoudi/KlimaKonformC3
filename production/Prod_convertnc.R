# this to convert asc files to netCDF format

library(stringr)
library(terra)

sim_steup <- read.csv("/media/HDD/Daten/WHK2/Data/raw_data/230509_setup 6ter Lauf.csv",
  stringsAsFactors = F
)[, c("run.id",  "crop.id",
      "gcm", "rcm", "scenario", "ensmem", "version", "start_date",
      "end_date", "irrigation",  "rcp")]

for(irow in 1:nrow(sim_steup)){
  if(sim_steup$scenario[irow]== "rcp26" & sim_steup$rcp[irow] == "rcp26"){
    sim_steup$NAME_1[irow] <- "0CO2"
  }
  if (sim_steup$scenario[irow]!= "rcp26" & sim_steup$rcp[irow] == "rcp26"){
    sim_steup$NAME_1[irow] <- "0CO2"
  }

  if (sim_steup$scenario[irow]!= "rcp26" & sim_steup$rcp[irow] != "rcp26"){
    sim_steup$NAME_1[irow] <- "1CO2"
  }

  if (sim_steup$irrigation[irow]== 1){
    sim_steup$NAME_2[irow] <- "1irrgiation"
  } else {
    sim_steup$NAME_2[irow] <- "0irrgiation"
  }


}
#expand.grid(c("rcp26", "rcp45", "rcp85"), c("rcp26", NA), c(TRUE, FALSE))

input_dir <- "/media/HDD/Daten/WHK2/Data/raw_data/6ter_Lauf"
output_dir <- "/media/HDD/Daten/WHK2/Data/netCDF/6ter_Lauf/"

# Create the  mode function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# list dirs
rcp_dirs <- list.dirs(input_dir)

# remove main dirs
rcp_dirs <- rcp_dirs[-which(str_count(rcp_dirs) != getmode(str_count(rcp_dirs)))]

# read standard output file
standard_output <- read.csv("/media/HDD/Daten/WHK2/Data/raw_data/2ter_Lauf/standard_output.csv",
  stringsAsFactors = F
)

# define time axis
time_axis1 <- seq(as.Date("1970/1/1", tz = "UTC"), as.Date("2099/1/1", tz = "UTC"), "years")
time_axis2 <- seq(as.Date("1971/1/1", tz = "UTC"), as.Date("2100/1/1", tz = "UTC"), "years")

i <- 1
j <- 6

# sim_steup$rcp[sim_steup$rcp == ""] <- 1
# sim_steup$rcp[sim_steup$rcp == "rcp26"] <- 0

# loop over rcp_dirs
for (i in 1:length(rcp_dirs)) {
  # obtain run_id
  blabla_strings <- unlist(str_split(rcp_dirs[i], "/"))

  exp_id <- blabla_strings[9]

  # obtain the simulation information
  run_index <- which(sim_steup$run.id == exp_id)

  # Water+CO2 flag
  CW_flag <- paste0(
    sim_steup$NAME_1[run_index],
    sim_steup$NAME_2[run_index]
  )

  # loop over variables
  for (j in 1:nrow(standard_output)) {
    asc.files <- list.files(
      path = rcp_dirs[i],
      pattern = standard_output$pattern[j],
      full.names = T,
      recursive = T
    )

    if (length(asc.files) == 0) {
      next
    } else {
      # print(length(asc.files))

      # convert to raster
      gibbon <- terra::rast(asc.files)

      # assign map projection
      crs(gibbon) <- "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

      indices_years <- (stringr::str_extract(
        names(gibbon),
        stringr::regex("(\\d+)(?!.*\\d)")
      ))

      indices_years <- as.numeric(indices_years)
      gibbon <- gibbon[[order(indices_years)]]

      # order_df<-as.data.frame(x=names(gibbon))
      # order_df$files<-asc.files
      #
      # for(stupid in 1:nrow(order_df)){
      #
      #   order_df$order[stupid] <- unlist(stringr::str_split(
      #     unlist(stringr::str_split(
      #       unlist(stringr::str_split(
      #         order_df$files[stupid], pattern = "/"))[11],
      #       pattern = "_"))[8], pattern = "[.]"))[1]
      #
      #
      # }
      #
      # gibbon<-terra::subset(gibbon,order(order_df$order))
      # order_df<-order_df[order(asc.files$order),c(1,2)]
      # assign time axix

      if (str_detect(names(gibbon)[length(names(gibbon))], "2099")) {
        terra::time(gibbon) <- as.POSIXct(time_axis2)
      } else {
        terra::time(gibbon) <- as.POSIXct(time_axis1)
      }


      if (standard_output$units[j] == "kg/ha") {
        # convert to dt/ha by dividing by 100
        gibbon <- gibbon / 100
        s <- sds(gibbon)
        units(s) <- "dt/ha"
      } else {
        s <- sds(gibbon)
        units(s) <- standard_output$units[j]
      }

      varnames(s) <- standard_output$variable[j]
      longnames(s) <- standard_output$longname[j]


      ncname <- paste0(
        sim_steup$run.id[run_index], "_", standard_output$variable[j], "_",
        sim_steup$gcm[run_index], "_", sim_steup$rcm[run_index], "_",
        sim_steup$scenario[run_index], "_", sim_steup$ensmem[run_index], "_",
        sim_steup$version[run_index], "_",
        as.Date(sim_steup$start_date[run_index],
          format = "%d/%m/%Y"
        ), "_",
        as.Date(sim_steup$end_date[run_index],
          format = "%d/%m/%Y"
        ),
        "_", CW_flag,
        ".nc"
      )

      ncpath <- paste0(output_dir, blabla_strings[9], "/")

      if (!dir.exists(ncpath)) dir.create(ncpath)

      y <- writeCDF(s, paste0(ncpath, ncname),
        zname = "time",
        compression = 9, overwrite = TRUE
      )
    }

    #
    print(exp_id)
  }
}

#
# copy missing from 2ter
#
# old_files<-list.files("/media/HDD/Daten/WHK2/Data/netCDF/2ter_lauf",
#            recursive = TRUE,
#            full.names = TRUE,
#            pattern = ".nc$")
#
# x<- as.character(sim_steup$run.id)
# sim_steup$run.id2 <- paste0(substr(x, 1,3), substr(x, 5,6))
#
# for (irow in 1: nrow(sim_steup)){
#
#   search.pattern<-paste0(sim_steup[irow, 3:7], collapse = "_")
#   files_indices <- which(Reduce("&", lapply(search.pattern, grepl, old_files, fixed = TRUE)))
#
#   search.pattern1 <- as.character(sim_steup$run.id2[irow])
#   files_indices <- which(Reduce("&", lapply(search.pattern1, grepl, old_files[files_indices], fixed = TRUE)))
#
#   if(sim_steup$scenario[irow]!= "rcp26")
#
# }




# result_df<-as.data.frame(x=NA)
# #loop over rcp_dirs
# for (i in 1:length(rcp_dirs)){
#   #loop over variables
#   for( j in 1:nrow(standard_output)){
#
#     asc.files<-list.files(path = rcp_dirs[i],
#                           pattern = standard_output$pattern[j], full.names = T,recursive = T)
#
#     if(length(asc.files)==0){
#
#       next
#     }else{
#       result_df$run_id[i]<-rcp_dirs[i]
#       result_df$pattern[j]<-rcp_dirs[i]
#       print(c(standard_output$pattern[j],length(asc.files)))
#     }
#   }
# }

# # WW ----------------------------------------------------------------------
# patterns<-c("AET_sum_year_",
# 						"Groundwater_recharge_sum_year_",
# 						"wheatwinterwheat_AET_sum_season_",
# 						"wheatwinterwheat_Groundwater_recharge_sum_season_",
# 						"wheatwinterwheat_LAI_max_season_",
# 						"wheatwinterwheat_Season_length_",
# 						"wheatwinterwheat_Yield_")
#
# variables<-c("AET",
# 						"GWR",
# 						"AET_WW",
# 						"GWR_WW",
# 						"LAI_max_WW",
# 						"sl_WW",
# 						"yield_WW")
#
# var_longnames<-c("AET_sum_year",
# 						"Groundwater_recharge_sum_year",
# 						"wheatwinterwheat_AET_sum_season_",
# 						"wheatwinterwheat_Groundwater_recharge_sum_season",
# 						"wheatwinterwheat_LAI_max_season",
# 						"wheatwinterwheat_Season_length",
# 						"wheatwinterwheat_Yield")
#
# units_string<-c("mm",
# 								"NA",
# 								"mm",
# 								"NA",
# 								"m2/m2",
# 								"days",
# 								"kg/ha")
#
#
#
# dirs<-dirs[-seq(2,length(dirs),2)]
#
#
#
# for (i in 1:length(dirs)){
#
# 	blabla_strings<- unlist(str_split(dirs[i],"/"))
#
# 	exp_id<-blabla_strings[10]
#
# for(j in 1:length(patterns)){
#
# 	asc.files<-list.files(path = paste0("/media/ahmed/Daten/WHK2/Data/2ter_Lauf/2ter_Lauf/26/",exp_id),
# 												pattern = patterns[j], full.names = T,recursive = T)
#
# 	print(length(asc.files))
#
# 	gibbon <- rast(asc.files)
#
#
#
# }
# }
#
#
# # SM ----------------------------------------------------------------------
#
#
# patterns<-c("AET_sum_year_",
# 						"Groundwater_recharge_sum_year_",
# 						"maizesilagemaize_AET_sum_season_",
# 						"maizesilagemaize_Groundwater_recharge_sum_season_",
# 						"maizesilagemaize_LAI_max_season_",
# 						"maizesilagemaize_Season_length_",
# 						"maizesilagemaize_Yield_")
#
# variables<-c("AET",
# 						 "GWR",
# 						 "AET_SM",
# 						 "GWR_SM",
# 						 "LAI_max_SM",
# 						 "sl_SM",
# 						 "yield_SM")
#
# var_longnames<-c("AET_sum_year",
# 								 "Groundwater_recharge_sum_year",
# 								 "maizesilagemaize_AET_sum_season_",
# 								 "maizesilagemaize_Groundwater_recharge_sum_season",
# 								 "maizesilagemaize_LAI_max_season",
# 								 "maizesilagemaize_Season_length",
# 								 "maizesilagemaize_Yield")
#
# units_string<-c("mm",
# 								"NA",
# 								"mm",
# 								"NA",
# 								"m2/m2",
# 								"days",
# 								"kg/ha")
#
# dirs<-list.dirs("/media/ahmed/Daten/WHK2/Data/2ter_Lauf/2ter_Lauf/26")[c(-1)]
#
# dirs<-dirs[-seq(1,length(dirs),2)]
#
#
#
# for (i in 1:length(dirs)){
#
# 	blabla_strings<- unlist(str_split(dirs[i],"/"))
#
# 	exp_id<-blabla_strings[10]
#
# 	for(j in 1:length(patterns)){
#
# 		asc.files<-list.files(path = paste0("/media/ahmed/Daten/WHK2/Data/2ter_Lauf/2ter_Lauf/26/",exp_id),
# 													pattern = patterns[j], full.names = T,recursive = T)
#
# 		print(length(asc.files))
#
# 		gibbon <- rast(asc.files)
#
# 		crs(gibbon)<-"+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#
# 		terra::time(gibbon)<-as.POSIXct(time_axis)
#
#
# 		s <- sds(gibbon)
# 		units(s) <- units_string[j]
# 		varnames(s) <- variables[j]
# 		longnames(s) <- var_longnames[j]
#
# 		run_index<-which(sim_steup$run.id==exp_id)
#
# 		ncname<-paste0(exp_id,"_",variables[j],"_",sim_steup$gcm[run_index],"_",sim_steup$rcm[run_index],".nc")
#
# 		ncpath<-paste0("/media/ahmed/Daten/WHK2/Data/Data_NetCDF/2ter_Lauf/26/")
#
# 		y <- writeCDF(s, paste0(ncpath,ncname), zname= "time",
# 									compression=9, overwrite=TRUE)
#
# 	}
# }
#
#
#
