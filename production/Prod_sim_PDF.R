library(stringr)
# 2ter --------------------------------------------------------------------

input_dir <- "/media/HDD/Daten/WHK2/Data/netCDF/2ter_lauf/"

nc.files <- list.files(
  path = input_dir,
  pattern = ".nc$",
  recursive = T,
  full.names = T
)

# now we are only intersted in the yield
nc.files <- nc.files[grep("yield", nc.files)]


output_path <- "/media/HDD/Volume/WHK2-tmp/Plotting/2ter_lauf"
output_plotting_data <- "/media/HDD/Volume/WHK2-tmp/Plotting_data/2ter_lauf"

# meta data
str_split_custom <- function(X) {
  first <- unlist(stringr::str_split(X, pattern = "/"))

  Xr <- unlist(stringr::str_split(first[length(first)], pattern = "_"))

  Xr[length(Xr)] <- unlist(stringr::str_split(Xr[length(Xr)], pattern = "[.]"))[1]

  return(Xr)
}

# convert NetCDF files to metada dataframe
meta_df_prod <- as.data.frame(do.call(rbind, lapply(nc.files, FUN = str_split_custom)))

regions <- c("total", "Vogtlandkreis", "Burgenlandkreis", "Greiz", "Altenburger Land")

landcover_variables <- c(1000, 211)


# get y.axis.limits


# output_path<-"D:/AHomoudi/KlimaKonform/output/Alle/"
ifile <- 1
iregion <- 2
ilandcover <- 2

for (ifile in 1:length(nc.files)) {
  # get var-path
  sub_var <- unlist(stringr::str_split(meta_df_prod$V2[ifile], "-"))

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
      if (stringr::str_detect(regions[iregion], pattern = " ")) {
        output_folder <- paste0(
          output_path, "/",
          landcover_variables[ilandcover], "/",
          stringr::str_replace(regions[iregion], " ", ""), "/",
          var_path, "/"
        )
        output_csv_folder <- paste0(
          output_plotting_data, "/",
          landcover_variables[ilandcover], "/",
          stringr::str_replace(regions[iregion], " ", ""), "/",
          var_path, "/"
        )
      } else {
        output_folder <- paste0(
          output_path, "/",
          landcover_variables[ilandcover], "/",
          regions[iregion], "/",
          var_path, "/"
        )
        output_csv_folder <- paste0(
          output_plotting_data, "/",
          landcover_variables[ilandcover], "/",
          regions[iregion], "/",
          var_path, "/"
        )
      }


      # create dir
      if (!dir.exists(output_folder)) dir.create(output_folder, recursive = T)
      if (!dir.exists(output_csv_folder)) dir.create(output_csv_folder, recursive = T)

      writeLines(text = c(
        nc.files[ifile],
        regions[iregion],
        landcover_variables[ilandcover],
        # y.axis.limit,
        "DE",
        "2ter",
        output_folder,
        output_csv_folder
      ), "input_text_PDF")

      system("R CMD BATCH sub_sim_PDF.R")
    }
  }
}

# 3ter --------------------------------------------------------------------

input_dir <- "/media/HDD/Daten/WHK2/Data/netCDF/3ter_lauf"

nc.files <- list.files(
  path = input_dir,
  pattern = ".nc$",
  recursive = T,
  full.names = T
)

# now we are only intersted in the yield
nc.files <- nc.files[grep("yield", nc.files)]


output_path <- "/media/HDD/Volume/WHK2-tmp/Plotting/3ter_lauf"
output_plotting_data <- "/media/HDD/Volume/WHK2-tmp/Plotting_data/3ter_lauf"

# meta data
str_split_custom <- function(X) {
  first <- unlist(stringr::str_split(X, pattern = "/"))

  Xr <- unlist(stringr::str_split(first[length(first)], pattern = "_"))

  Xr[length(Xr)] <- unlist(stringr::str_split(Xr[length(Xr)], pattern = "[.]"))[1]

  return(Xr)
}

# convert NetCDF files to metada dataframe
meta_df_prod <- as.data.frame(do.call(rbind, lapply(nc.files, FUN = str_split_custom)))

regions <- c("total", "Vogtlandkreis", "Burgenlandkreis", "Greiz", "Altenburger Land")

landcover_variables <- c(1000, 211)


# get y.axis.limits


# output_path<-"D:/AHomoudi/KlimaKonform/output/Alle/"
ifile <- 1
iregion <- 2
ilandcover <- 2

for (ifile in 1:length(nc.files)) {
  # get var-path
  sub_var <- unlist(stringr::str_split(meta_df_prod$V2[ifile], "-"))

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
      if (stringr::str_detect(regions[iregion], pattern = " ")) {
        output_folder <- paste0(
          output_path, "/",
          landcover_variables[ilandcover], "/",
          stringr::str_replace(regions[iregion], " ", ""), "/",
          var_path, "/"
        )
        output_csv_folder <- paste0(
          output_plotting_data, "/",
          landcover_variables[ilandcover], "/",
          stringr::str_replace(regions[iregion], " ", ""), "/",
          var_path, "/"
        )
      } else {
        output_folder <- paste0(
          output_path, "/",
          landcover_variables[ilandcover], "/",
          regions[iregion], "/",
          var_path, "/"
        )
        output_csv_folder <- paste0(
          output_plotting_data, "/",
          landcover_variables[ilandcover], "/",
          regions[iregion], "/",
          var_path, "/"
        )
      }


      # create dir
      if (!dir.exists(output_folder)) dir.create(output_folder, recursive = T)
      if (!dir.exists(output_csv_folder)) dir.create(output_csv_folder, recursive = T)

      writeLines(text = c(
        nc.files[ifile],
        regions[iregion],
        landcover_variables[ilandcover],
        # y.axis.limit,
        "DE",
        "3ter",
        output_folder,
        output_csv_folder
      ), "input_text_PDF")

      system("R CMD BATCH sub_sim_PDF.R")
    }
  }
}
# 4ter --------------------------------------------------------------------

input_dir <- "/media/HDD/Daten/WHK2/Data/netCDF/4ter_lauf"

nc.files <- list.files(
  path = input_dir,
  pattern = ".nc$",
  recursive = T,
  full.names = T
)

# now we are only intersted in the yield
nc.files <- nc.files[grep("yield", nc.files)]


output_path <- "/media/HDD/Volume/WHK2-tmp/Plotting/4ter_lauf"
output_plotting_data <- "/media/HDD/Volume/WHK2-tmp/Plotting_data/4ter_lauf"

# meta data
str_split_custom <- function(X) {
  first <- unlist(stringr::str_split(X, pattern = "/"))

  Xr <- unlist(stringr::str_split(first[length(first)], pattern = "_"))

  Xr[length(Xr)] <- unlist(stringr::str_split(Xr[length(Xr)], pattern = "[.]"))[1]

  return(Xr)
}

# convert NetCDF files to metada dataframe
meta_df_prod <- as.data.frame(do.call(rbind, lapply(nc.files, FUN = str_split_custom)))

regions <- c("total", "Vogtlandkreis", "Burgenlandkreis", "Greiz", "Altenburger Land")

landcover_variables <- c(1000, 211)


# get y.axis.limits


# output_path<-"D:/AHomoudi/KlimaKonform/output/Alle/"
ifile <- 1
iregion <- 2
ilandcover <- 2

for (ifile in 1:length(nc.files)) {
  # get var-path
  sub_var <- unlist(stringr::str_split(meta_df_prod$V2[ifile], "-"))

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
      if (stringr::str_detect(regions[iregion], pattern = " ")) {
        output_folder <- paste0(
          output_path, "/",
          landcover_variables[ilandcover], "/",
          stringr::str_replace(regions[iregion], " ", ""), "/",
          var_path, "/"
        )
        output_csv_folder <- paste0(
          output_plotting_data, "/",
          landcover_variables[ilandcover], "/",
          stringr::str_replace(regions[iregion], " ", ""), "/",
          var_path, "/"
        )
      } else {
        output_folder <- paste0(
          output_path, "/",
          landcover_variables[ilandcover], "/",
          regions[iregion], "/",
          var_path, "/"
        )
        output_csv_folder <- paste0(
          output_plotting_data, "/",
          landcover_variables[ilandcover], "/",
          regions[iregion], "/",
          var_path, "/"
        )
      }


      # create dir
      if (!dir.exists(output_folder)) dir.create(output_folder, recursive = T)
      if (!dir.exists(output_csv_folder)) dir.create(output_csv_folder, recursive = T)

      writeLines(text = c(
        nc.files[ifile],
        regions[iregion],
        landcover_variables[ilandcover],
        # y.axis.limit,
        "DE",
        "4ter",
        output_folder,
        output_csv_folder
      ), "input_text_PDF")

      system("R CMD BATCH sub_sim_PDF.R")
    }
  }
}
