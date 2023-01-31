library(stringr)
# 2ter --------------------------------------------------------------------

input_dir <- "/media/ahmed/Daten/WHK2/Data/netCDF/2ter_lauf_ensemble"

nc.files <- list.files(
  path = input_dir,
  pattern = ".nc$",
  recursive = T,
  full.names = T
)
stat_variables <- c("mean", "sd", "median", "max", "min")

output_path <- "/media/ahmed/Volume/WHK2-tmp/Plotting/2ter_lauf_ensemble"
output_plotting_data <- "/media/ahmed/Volume/WHK2-tmp/Plotting_data/2ter_lauf_ensemble"
# meta data
str_split_custom <- function(X) {
  first <- unlist(stringr::str_split(X, pattern = "/"))

  Xr <- unlist(stringr::str_split(first[length(first)], pattern = "_"))

  Xr[length(Xr)] <- unlist(stringr::str_split(Xr[length(Xr)], pattern = "[.]"))[1]

  return(Xr)
}

# convert NetCDF files to metada dataframe
meta_df_prod <- as.data.frame(do.call(rbind, lapply(nc.files, FUN = str_split_custom)))

vars <- unique(meta_df_prod[c("V2")])

regions <- c("total", "Vogtlandkreis", "Burgenlandkreis", "Greiz", "Altenburger Land")

stat_variables <- c("mean", "sd", "median", "max", "min")

landcover_variables <- c(1000, 211)

# output_path<-"D:/AHomoudi/KlimaKonform/output/Alle/"
ivar <- 1
iregion <- 2
istat <- 1
ilandcover <- 2


# loop over uniqe variable as AET
for (ivar in 1:nrow(vars)) {
  sub_files <- grep(pattern = paste0(
    "_",
    vars[ivar, 1],
    "_"
  ), x = nc.files, value = T)

  # get var-path
  sub_var <- unlist(stringr::str_split(vars[ivar, 1], "-"))
  if (length(sub_var) == 2) {
    if (sub_var[1] == "yield") {
      sub_var[1] <- stringr::str_to_title(sub_var[1])
    }

    var_path <- paste0(sub_var[1], "/", sub_var[1], "-", sub_var[2])
  } else {
    if (vars[ivar, 1] == "yield") {
      vars[ivar, 1] <- stringr::str_to_title(vars[ivar, 1])
    }
    var_path <- paste0(vars[ivar, 1], "/", vars[ivar, 1])
  }

  # loop over regions
  for (iregion in 1:length(regions)) {
    # loop over landcover
    for (ilandcover in 1:length(landcover_variables)) {
      # loop over statistical ensemble members

      for (istat in 1:length(stat_variables)) {
        if (stringr::str_detect(regions[iregion], pattern = " ")) {
          output_folder <- paste0(
            output_path, "/",
            landcover_variables[ilandcover], "/",
            stringr::str_replace(regions[iregion], " ", ""), "/",
            stat_variables[istat], "/",
            var_path, "/"
          )
          output_csv_folder <- paste0(
            output_plotting_data, "/",
            landcover_variables[ilandcover], "/",
            stringr::str_replace(regions[iregion], " ", ""), "/",
            stat_variables[istat], "/",
            var_path, "/"
          )
        } else {
          output_folder <- paste0(
            output_path, "/",
            landcover_variables[ilandcover], "/",
            regions[iregion], "/",
            stat_variables[istat], "/",
            var_path, "/"
          )
          output_csv_folder <- paste0(
            output_plotting_data, "/",
            landcover_variables[ilandcover], "/",
            regions[iregion], "/",
            stat_variables[istat], "/",
            var_path, "/"
          )
        }


        # create dir
        if (!dir.exists(output_folder)) dir.create(output_folder, recursive = T)
        if (!dir.exists(output_csv_folder)) dir.create(output_csv_folder, recursive = T)

        print(c(
          output_folder,
          output_csv_folder
        ))

        writeLines(text = c(
          sub_files,
          vars[ivar, 1],
          regions[iregion],
          landcover_variables[ilandcover],
          "DE",
          stat_variables[istat],
          "2ter",
          output_folder,
          output_csv_folder
        ), "input_text_density")

        system("R CMD BATCH sub_ens_density.R")

        gc()
      }
    }
  }
}
# test
# netCDF.files <- sub_files
# variable <- vars[ivar, 1]
# region <- regions[iregion]
# landcover <- landcover_variables[ilandcover]
# language <- "DE"
# run_id <-"2ter"
# stat_var <- stat_variables[istat]
# output_path <- output_folder
# output_csv<-output_csv_folder
# 3ter --------------------------------------------------------------------

input_dir <- "/media/ahmed/Daten/WHK2/Data/netCDF/3ter_lauf_ensemble"

nc.files <- list.files(
  path = input_dir,
  pattern = ".nc$",
  recursive = T,
  full.names = T
)

# remove SOC files
nc.files <- nc.files[-grep("_SOC_", nc.files)]
stat_variables <- c("mean", "sd", "median", "max", "min")

output_path <- "/media/ahmed/Volume/WHK2-tmp/Plotting/3ter_lauf_ensemble"
output_plotting_data <- "/media/ahmed/Volume/WHK2-tmp/Plotting_data/3ter_lauf_ensemble"
# meta data
str_split_custom <- function(X) {
  first <- unlist(stringr::str_split(X, pattern = "/"))

  Xr <- unlist(stringr::str_split(first[length(first)], pattern = "_"))

  Xr[length(Xr)] <- unlist(stringr::str_split(Xr[length(Xr)], pattern = "[.]"))[1]

  return(Xr)
}

# convert NetCDF files to metada dataframe
meta_df_prod <- as.data.frame(do.call(rbind, lapply(nc.files, FUN = str_split_custom)))

vars <- unique(meta_df_prod[c("V2")])

regions <- c("total", "Vogtlandkreis", "Burgenlandkreis", "Greiz", "Altenburger Land")

stat_variables <- c("mean", "sd", "median", "max", "min")

landcover_variables <- c(1000, 211)

# output_path<-"D:/AHomoudi/KlimaKonform/output/Alle/"
ivar <- 1
iregion <- 2
istat <- 1
ilandcover <- 2


# loop over uniqe variable as AET
for (ivar in 1:nrow(vars)) {
  sub_files <- grep(pattern = paste0(
    "_",
    vars[ivar, 1],
    "_"
  ), x = nc.files, value = T)

  # get var-path
  sub_var <- unlist(stringr::str_split(vars[ivar, 1], "-"))
  if (length(sub_var) == 2) {
    if (sub_var[1] == "yield") {
      sub_var[1] <- stringr::str_to_title(sub_var[1])
    }

    var_path <- paste0(sub_var[1], "/", sub_var[1], "-", sub_var[2])
  } else {
    if (vars[ivar, 1] == "yield") {
      vars[ivar, 1] <- stringr::str_to_title(vars[ivar, 1])
    }
    var_path <- paste0(vars[ivar, 1], "/", vars[ivar, 1])
  }

  # loop over regions
  for (iregion in 1:length(regions)) {
    # loop over landcover
    for (ilandcover in 1:length(landcover_variables)) {
      # loop over statistical ensemble memebers

      for (istat in 1:length(stat_variables)) {
        if (stringr::str_detect(regions[iregion], pattern = " ")) {
          output_folder <- paste0(
            output_path, "/",
            landcover_variables[ilandcover], "/",
            stringr::str_replace(regions[iregion], " ", ""), "/",
            stat_variables[istat], "/",
            var_path, "/"
          )
          output_csv_folder <- paste0(
            output_plotting_data, "/",
            landcover_variables[ilandcover], "/",
            stringr::str_replace(regions[iregion], " ", ""), "/",
            stat_variables[istat], "/",
            var_path, "/"
          )
        } else {
          output_folder <- paste0(
            output_path, "/",
            landcover_variables[ilandcover], "/",
            regions[iregion], "/",
            stat_variables[istat], "/",
            var_path, "/"
          )
          output_csv_folder <- paste0(
            output_plotting_data, "/",
            landcover_variables[ilandcover], "/",
            regions[iregion], "/",
            stat_variables[istat], "/",
            var_path, "/"
          )
        }


        # create dir
        if (!dir.exists(output_folder)) dir.create(output_folder, recursive = T)
        if (!dir.exists(output_csv_folder)) dir.create(output_csv_folder, recursive = T)

        print(c(
          output_folder,
          output_csv_folder
        ))

        writeLines(text = c(
          sub_files,
          vars[ivar, 1],
          regions[iregion],
          landcover_variables[ilandcover],
          "DE",
          stat_variables[istat],
          "3ter",
          output_folder,
          output_csv_folder
        ), "input_text_density")

        system("R CMD BATCH sub_ens_density.R")

        gc()
      }
    }
  }
}
# test
# netCDF.files <- sub_files
# variable <- vars[ivar, 1]
# region <- regions[iregion]
# landcover <- landcover_variables[ilandcover]
# language <- "DE"
# run_id <-"2ter"
# stat_var <- stat_variables[istat]
# output_path <- output_folder
# output_csv<-output_csv_folder
# 4ter --------------------------------------------------------------------

input_dir <- "/media/ahmed/Daten/WHK2/Data/netCDF/4ter_lauf_ensemble"

nc.files <- list.files(
  path = input_dir,
  pattern = ".nc$",
  recursive = T,
  full.names = T
)

# remove SOC files
nc.files <- nc.files[-grep("_SOC_", nc.files)]
stat_variables <- c("mean", "sd", "median", "max", "min")

output_path <- "/media/ahmed/Volume/WHK2-tmp/Plotting/4ter_lauf_ensemble"
output_plotting_data <- "/media/ahmed/Volume/WHK2-tmp/Plotting_data/4ter_lauf_ensemble"
# meta data
str_split_custom <- function(X) {
  first <- unlist(stringr::str_split(X, pattern = "/"))

  Xr <- unlist(stringr::str_split(first[length(first)], pattern = "_"))

  Xr[length(Xr)] <- unlist(stringr::str_split(Xr[length(Xr)], pattern = "[.]"))[1]

  return(Xr)
}

# convert NetCDF files to metada dataframe
meta_df_prod <- as.data.frame(do.call(rbind, lapply(nc.files, FUN = str_split_custom)))

vars <- unique(meta_df_prod[c("V2")])

regions <- c("total", "Vogtlandkreis", "Burgenlandkreis", "Greiz", "Altenburger Land")

stat_variables <- c("mean", "sd", "median", "max", "min")

landcover_variables <- c(1000, 211)

# output_path<-"D:/AHomoudi/KlimaKonform/output/Alle/"
ivar <- 1
iregion <- 2
istat <- 1
ilandcover <- 2


# loop over uniqe variable as AET
for (ivar in 1:nrow(vars)) {
  sub_files <- grep(pattern = paste0(
    "_",
    vars[ivar, 1],
    "_"
  ), x = nc.files, value = T)

  # get var-path
  sub_var <- unlist(stringr::str_split(vars[ivar, 1], "-"))
  if (length(sub_var) == 2) {
    if (sub_var[1] == "yield") {
      sub_var[1] <- stringr::str_to_title(sub_var[1])
    }

    var_path <- paste0(sub_var[1], "/", sub_var[1], "-", sub_var[2])
  } else {
    if (vars[ivar, 1] == "yield") {
      vars[ivar, 1] <- stringr::str_to_title(vars[ivar, 1])
    }
    var_path <- paste0(vars[ivar, 1], "/", vars[ivar, 1])
  }

  # loop over regions
  for (iregion in 1:length(regions)) {
    # loop over landcover
    for (ilandcover in 1:length(landcover_variables)) {
      # loop over statistical ensemble memebers

      for (istat in 1:length(stat_variables)) {
        if (stringr::str_detect(regions[iregion], pattern = " ")) {
          output_folder <- paste0(
            output_path, "/",
            landcover_variables[ilandcover], "/",
            stringr::str_replace(regions[iregion], " ", ""), "/",
            stat_variables[istat], "/",
            var_path, "/"
          )
          output_csv_folder <- paste0(
            output_plotting_data, "/",
            landcover_variables[ilandcover], "/",
            stringr::str_replace(regions[iregion], " ", ""), "/",
            stat_variables[istat], "/",
            var_path, "/"
          )
        } else {
          output_folder <- paste0(
            output_path, "/",
            landcover_variables[ilandcover], "/",
            regions[iregion], "/",
            stat_variables[istat], "/",
            var_path, "/"
          )
          output_csv_folder <- paste0(
            output_plotting_data, "/",
            landcover_variables[ilandcover], "/",
            regions[iregion], "/",
            stat_variables[istat], "/",
            var_path, "/"
          )
        }


        # create dir
        if (!dir.exists(output_folder)) dir.create(output_folder, recursive = T)
        if (!dir.exists(output_csv_folder)) dir.create(output_csv_folder, recursive = T)

        print(c(
          output_folder,
          output_csv_folder
        ))

        writeLines(text = c(
          sub_files,
          vars[ivar, 1],
          regions[iregion],
          landcover_variables[ilandcover],
          "DE",
          stat_variables[istat],
          "4ter",
          output_folder,
          output_csv_folder
        ), "input_text_density")

        system("R CMD BATCH sub_ens_density.R")

        gc()
      }
    }
  }
}
# test
# netCDF.files <- sub_files
# variable <- vars[ivar, 1]
# region <- regions[iregion]
# landcover <- landcover_variables[ilandcover]
# language <- "DE"
# run_id <-"2ter"
# stat_var <- stat_variables[istat]
# output_path <- output_folder
# output_csv<-output_csv_folder
