library(stringr)
library(KlimaKonformC3)

# rename files ------------------------------------------------------------
# input_dir1 <- "/media/HDD/Daten/WHK2/Data/netCDF/2ter_lauf"
# nc.files1 <- list.files(
#   path = input_dir1,
#   pattern = ".nc$",
#   recursive = T,
#   full.names = T
# )
#
# lapply(nc.files1, FUN = function(ifile) {
#   new.filename <- gsub(".nc$", "_1CO20irrigation.nc", ifile)
#   file.copy(from = ifile, to = new.filename)
# })
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
nc.files <- nc.files[grep("yield", nc.files)]


output_path <- "/media/HDD/Volume/WHK2-tmp/Plotting/6ter_lauf/boxplots"
output_plotting_data <- "/media/HDD/Volume/WHK2-tmp/Plotting_data/6ter_lauf"

# meta data
str_split_custom <- function(X) {
  first <- unlist(stringr::str_split(X, pattern = "/"))

  Xr <- unlist(stringr::str_split(first[length(first)], pattern = "_"))

  Xr[length(Xr)] <- unlist(stringr::str_split(Xr[length(Xr)], pattern = "[.]"))[1]

  return(Xr)
}

# convert NetCDF files to metada dataframe
meta_df_prod <- as.data.frame(do.call(
  rbind,
  lapply(nc.files,
    FUN = str_split_custom
  )
))

meta_df_prod$V11 <- substr(meta_df_prod$V1, 1, 5)

meta_unique <- apply(unique(meta_df_prod[2:7]),
  MARGIN = 1, FUN = function(x) {
    paste0(x, collapse = "_")
  }
)

# combine the four files
regions <- c("total", "Vogtlandkreis", "Burgenlandkreis", "Greiz", "Altenburger Land")

landcover_variables <- c(211)
# landcover_variables <- c(1000, 211)

# used cores
ncores <- parallel::detectCores(logical = FALSE) / 3
print(ncores)
# loop over files
parallel::mclapply(1:length(meta_unique), function(igroup) {
  # get sub files
  sub_files <- grep(
    pattern = meta_unique[igroup],
    nc.files,
    value = TRUE
  )
  # get meta data
  sub_metadf <- as.data.frame(do.call(
    rbind,
    lapply(sub_files,
      FUN = str_split_custom
    )
  ))

  # get var-path
  sub_var <- unlist(stringr::str_split(sub_metadf$V2[1], "-"))

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
      if (!dir.exists(output_folder)) {
        dir.create(output_folder,
          recursive = T
        )
      }
      if (!dir.exists(output_csv_folder)) {
        dir.create(output_csv_folder,
          recursive = T
        )
      }

      exp_boxplots(
        netCDF.files = sub_files,
        variable = unique(sub_metadf$V2),
        region = regions[iregion],
        landcover = landcover_variables[ilandcover],
        language = "DE",
        run_id = "6ter",
        output_path = output_folder,
        output_csv = NULL
      )

      # writeLines(text = c(
      #   sub_files,
      #   unique(sub_metadf$V2),
      #   regions[iregion],
      #   landcover_variables[ilandcover],
      #   # y.axis.limit,
      #   "DE",
      #   "6ter",
      #   output_folder,
      #   output_csv_folder
      # ), "input_text_boxplots")
      #
      # system("R CMD BATCH sub_exp_boxplots.R")
    }
  }
}, mc.cores = ncores)
# output_path<-"D:/AHomoudi/KlimaKonform/output/Alle/"
# ifile <- 1
# iregion <- 2
# ilandcover <- 1
# igroup<-1
# for (igroup in 1:length(meta_unique)) {
#
# }

# rename files ------------------------------------------------------------
# input_dir1 <- "/media/HDD/Daten/WHK2/Data/netCDF/2ter_lauf"
# nc.files1 <- list.files(
#   path = input_dir1,
#   pattern = "_1CO20irrigation.nc",
#   recursive = T,
#   full.names = T
# )
#
# file.remove(nc.files1)
