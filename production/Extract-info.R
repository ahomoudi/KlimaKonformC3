
library(stringr)
library(terra)
# 2ter --------------------------------------------------------------------

input_dir <- "/media/ahmed/Daten/WHK2/Data/netCDF/2ter_lauf"

nc.files <- list.files(
  path = input_dir,
  pattern = ".nc$",
  recursive = T,
  full.names = T
)

str_split_custom <- function(X) {
  first <- unlist(stringr::str_split(X, pattern = "/"))

  Xr <- unlist(stringr::str_split(first[length(first)], pattern = "_"))

  Xr[length(Xr)] <- unlist(stringr::str_split(Xr[length(Xr)], pattern = "[.]"))[1]

  return(Xr)
}

# convert NetCDF files to metada dataframe
meta_df_prod <- as.data.frame(do.call(rbind, lapply(nc.files, FUN = str_split_custom)))

start_time <- Sys.time()
minmax_df<-lapply(nc.files, function(x){

  r<-terra::rast(x)

  y<-c(min(terra::values(r), na.rm = T),
       max(terra::values(r), na.rm = T))

  rm(r);gc()
  return(y)
})

minmax_df<-Reduce(minmax_df, f = rbind)

meta_df_prod<-cbind(meta_df_prod,minmax_df)
end_time <- Sys.time()
end_time - start_time

# for(ifile in 1:length(nc.files)){
#   r<-terra::rast(nc.files[ifile])
#
#   meta_df_prod$minimum[ifile]<-min(terra::values(r), na.rm = T)
#   meta_df_prod$maximum[ifile]<-max(terra::values(r), na.rm = T)
#
# rm(r);gc()
# }
colnames(meta_df_prod)<-c("sim_id","variable","GCM","RCM", "rcp",
                          "ensemble", "version", "start", "end","minimum","maximum")
data.table::fwrite(meta_df_prod,
                   "~/CloudStore/Nextcloud/Shared/KlimaKonform-Results/info_simulations/2ter_lauf.csv")
# 3ter --------------------------------------------------------------------

input_dir <- "/media/ahmed/Daten/WHK2/Data/netCDF/3ter_lauf"

nc.files <- list.files(
  path = input_dir,
  pattern = ".nc$",
  recursive = T,
  full.names = T
)

# remove SOC files
nc.files<-nc.files[-grep("_SOC_", nc.files)]


# convert NetCDF files to metada dataframe
meta_df_prod <- as.data.frame(do.call(rbind, lapply(nc.files, FUN = str_split_custom)))

start_time <- Sys.time()

for(ifile in 1:length(nc.files)){
  r<-terra::rast(nc.files[ifile])

  meta_df_prod$minimum[ifile]<-min(terra::values(r), na.rm = T)
  meta_df_prod$maximum[ifile]<-max(terra::values(r), na.rm = T)

  rm(r);gc()
}

end_time <- Sys.time()
end_time - start_time

colnames(meta_df_prod)<-c("sim_id","variable","GCM","RCM", "rcp",
                          "ensemble", "version","start", "end","minimum","maximum")
data.table::fwrite(meta_df_prod,
                   "~/CloudStore/Nextcloud/Shared/KlimaKonform-Results/info_simulations/3ter_lauf.csv")

# 4ter --------------------------------------------------------------------
# fix names first
dir_path<-"/media/ahmed/Daten/WHK2/Data/netCDF/4ter_lauf"

#rcp26_SOC_10_ensemble-stats_01.01.1970_31.12.2099
ncfiles<-list.files(path = dir_path,
                    full.names = T,
                    recursive = T,
                    pattern = ".nc$")

ncfiles<-grep(pattern = "01.01.1970_31.12.2099",
              value = T,
              x = ncfiles)

for (iii in 1:length(ncfiles)){

  newname<-gsub(pattern = "01.01.1970_31.12.2099",
                replacement = "1970-01-01_2099-12-31",
                x = ncfiles[iii])

  file.rename(from = ncfiles[iii], to = newname)
}


#
input_dir <- "/media/ahmed/Daten/WHK2/Data/netCDF/4ter_lauf"

nc.files <- list.files(
  path = input_dir,
  pattern = ".nc$",
  recursive = T,
  full.names = T
)

# remove SOC files
nc.files<-nc.files[-grep("_SOC_", nc.files)]

# convert NetCDF files to metada dataframe
meta_df_prod <- as.data.frame(do.call(rbind, lapply(nc.files, FUN = str_split_custom)))

start_time <- Sys.time()
minmax_df<-lapply(nc.files, function(x){

  r<-terra::rast(x)

  y<-c(min(terra::values(r), na.rm = T),
       max(terra::values(r), na.rm = T))

  #rm(r);gc()
  return(y)
})

minmax_df<-Reduce(minmax_df, f = rbind)

meta_df_prod<-cbind(meta_df_prod,minmax_df)
end_time <- Sys.time()
end_time - start_time

colnames(meta_df_prod)<-c("sim_id","variable","GCM","RCM", "rcp",
                          "ensemble", "version","start", "end","minimum","maximum")
data.table::fwrite(meta_df_prod %>%
                     dplyr::mutate_if(is.numeric, round, digits = 3),
                   "~/CloudStore/Nextcloud/Shared/KlimaKonform-Results/info_simulations/4ter_lauf.csv")


#ätest
rbenchmark::benchmark("values" = {
  x<-max(r[1:ncell(r)], na.rm = T)


},

"access" = {

  y <- max(terra::values(r), na.rm = T)

},
replications = 10)

# 6ter --------------------------------------------------------------------

input_dir1 <- "/media/HDD/Daten/WHK2/Data/netCDF/2ter_lauf"
input_dir2 <- "/media/HDD/Daten/WHK2/Data/netCDF/6ter_Lauf"

nc.files1 <- list.files(
  path = input_dir1,
  pattern = "1CO20irrigation.nc$",
  recursive = T,
  full.names = T
)

nc.files2 <- list.files(
  path = input_dir2,
  pattern = ".nc$",
  recursive = T,
  full.names = T
)

# now we are only interested in the yield
nc.files <- c(nc.files1, nc.files2)

str_split_custom <- function(X) {
  first <- unlist(stringr::str_split(X, pattern = "/"))

  Xr <- unlist(stringr::str_split(first[length(first)], pattern = "_"))

  Xr[length(Xr)] <- unlist(stringr::str_split(Xr[length(Xr)], pattern = "[.]"))[1]

  return(Xr)
}

# convert NetCDF files to metada dataframe
meta_df_prod <- as.data.frame(do.call(rbind, lapply(nc.files, FUN = str_split_custom)))

# used cores
ncores <- parallel::detectCores(logical = FALSE) - 2
print(ncores)

start_time <- Sys.time()
minmax_df<-parallel::mclapply(nc.files, function(x){

  r<-terra::rast(x)

  y<-c(min(terra::values(r), na.rm = T),
       max(terra::values(r), na.rm = T))

  rm(r);gc()
  return(y)
}, mc.cores = ncores)

minmax_df<-Reduce(minmax_df, f = rbind)

meta_df_prod<-cbind(meta_df_prod,minmax_df)
end_time <- Sys.time()
end_time - start_time

# colnames(meta_df_prod)<-c("sim_id","variable","GCM","RCM", "rcp",
#                           "ensemble", "version", "start", "end","minimum","maximum")
data.table::fwrite(meta_df_prod,
                   "~/CloudStore/Nextcloud/Shared/KlimaKonform-Results/info_simulations/6ter_lauf.csv")
