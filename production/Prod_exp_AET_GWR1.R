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
# 6ter V002 --------------------------------------------------------------------

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
nc.files <- nc.files[c(
  grep("_AET_", nc.files),
  grep("_GWR_", nc.files)
)]

# output_path <- "~/NextCloud/DATA/Shared/KlimaKonform-Results/Plotting/6ter_lauf/ts"
output_path <- "/home/ahmed/NextCloud/DATA/Shared/KlimaKonform-Results/Plotting/6ter_lauf/ts"
output_plotting_data <- "/media/HDD/Volume/WHK2-tmp/Plotting_data/6ter_lauf"

# meta data
str_split_custom <- function(X) {
  first <- unlist(stringr::str_split(X, pattern = "/"))

  Xr <- unlist(stringr::str_split(first[length(first)],
    pattern = "_"
  ))

  Xr[length(Xr)] <- unlist(stringr::str_split(Xr[length(Xr)],
    pattern = "[.]"
  ))[1]

  return(Xr)
}

# convert NetCDF files to metada dataframe
meta_df_prod <- as.data.frame(do.call(
  rbind,
  lapply(nc.files,
    FUN = str_split_custom
  )
))

meta_df_prod$V11 <- substr(
  meta_df_prod$V1,
  1, 5
)

sim_steup <- rbind(
  read.csv("/media/HDD/Daten/WHK2/Data/raw_data/230509_setup 6ter Lauf.csv",
    stringsAsFactors = F
  )[, c("run.id", "crop.id")],
  read.csv("/media/HDD/Daten/WHK2/Data/raw_data/2ter_Lauf/sim_setups_neu.csv",
    stringsAsFactors = F
  )[, c("run.id", "crop.id")]
)

sim_steup$run.id <- as.character(sim_steup$run.id)

meta_df_prod <- meta_df_prod %>%
  dplyr::rename(run.id = V1) %>%
  dplyr::left_join(y = sim_steup, by = "run.id")

meta_unique <- unique(meta_df_prod[c(2:7, 12)])

# combine the four files
regions <- c(
  "total", "Vogtlandkreis",
  "Burgenlandkreis", "Greiz",
  "Altenburger Land"
)

landcover_variables <- c(211)
# landcover_variables <- c(1000, 211)

# used cores
ncores <- parallel::detectCores(logical = FALSE) / 3
print(ncores)


# loop over files
parallel::mclapply(1:nrow(meta_unique), function(igroup) {
  patterns_df <- meta_df_prod %>%
    dplyr::filter(V2 == meta_unique$V2[igroup]) %>%
    dplyr::filter(V3 == meta_unique$V3[igroup]) %>%
    dplyr::filter(V4 == meta_unique$V4[igroup]) %>%
    dplyr::filter(V5 == meta_unique$V5[igroup]) %>%
    dplyr::filter(V6 == meta_unique$V6[igroup]) %>%
    dplyr::filter(crop.id == meta_unique$crop.id[igroup])

  vars_uniqui <- unique(patterns_df$crop.id)

  patterns_df <- apply(patterns_df[, 1:9], MARGIN = 1, function(x) {
    paste0(x, collapse = "_")
  })

  # get sub files
  sub_files <- unlist(lapply(patterns_df, FUN = function(x) {
    grep(x,
      nc.files,
      value = T
    )
  }))
  # get meta data
  sub_metadf <- as.data.frame(do.call(
    rbind,
    lapply(sub_files,
      FUN = str_split_custom
    )
  ))

  vars <- unique(sub_metadf$V2)
  ivar <- 1

  # get var-path
  sub_var <- unlist(stringr::str_split(sub_metadf$V2[1], "-"))
  var_path <- paste0(sub_var[1], "/", sub_var[1], "from", vars_uniqui)

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

      tmp_file <- paste0(
        "input_LT_",
        igroup,
        unique(sub_metadf$V2),
        ilandcover,
        iregion
      )

      # exp_linear_trend(
      #   netCDF.files = sub_files,
      #   variable = unique(sub_metadf$V2),
      #   region = regions[iregion],
      #   landcover = landcover_variables[ilandcover],
      #   language = "DE",
      #   run_id = "6ter",
      #   output_path = output_folder,
      #   output_csv = NULL
      # )

      writeLines(text = c(
        sub_files,
        unique(sub_metadf$V2),
        regions[iregion],
        landcover_variables[ilandcover],
        # y.axis.limit,
        "DE",
        "6ter",
        output_folder,
        output_csv_folder
      ), tmp_file)

      # sub_exp_linear_trend.R

      system(paste0(
        "Rscript --vanilla sub_exp_linear_trend.R ",
        tmp_file
      ))

      file.remove(tmp_file)
    }
  }
}, mc.cores = ncores)
# output_path<-"D:/AHomoudi/KlimaKonform/output/Alle/"
# ifile <- 1
# iregion <- 2
# ilandcover <- 1
# igroup <- 1
# 6ter V002 --------------------------------------------------------------------

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
nc.files <- nc.files[c(
  grep("_AET_", nc.files),
  grep("_GWR_", nc.files)
)]


# output_path <- "~/NextCloud/DATA/Shared/KlimaKonform-Results/Plotting/6ter_lauf/boxplots"
output_path <- "/media/HDD/Volume/WHK2-tmp/Plotting/6ter_lauf/boxplots"
output_plotting_data <- "/media/HDD/Volume/WHK2-tmp/Plotting_data/6ter_lauf"

# meta data
str_split_custom <- function(X) {
  first <- unlist(stringr::str_split(X, pattern = "/"))

  Xr <- unlist(stringr::str_split(first[length(first)],
    pattern = "_"
  ))

  Xr[length(Xr)] <- unlist(stringr::str_split(Xr[length(Xr)],
    pattern = "[.]"
  ))[1]

  return(Xr)
}

# convert NetCDF files to metada dataframe
meta_df_prod <- as.data.frame(do.call(
  rbind,
  lapply(nc.files,
    FUN = str_split_custom
  )
))

meta_df_prod$V11 <- substr(
  meta_df_prod$V1,
  1, 5
)

sim_steup <- rbind(
  read.csv("/media/HDD/Daten/WHK2/Data/raw_data/230509_setup 6ter Lauf.csv",
    stringsAsFactors = F
  )[, c("run.id", "crop.id")],
  read.csv("/media/HDD/Daten/WHK2/Data/raw_data/2ter_Lauf/sim_setups_neu.csv",
    stringsAsFactors = F
  )[, c("run.id", "crop.id")]
)

sim_steup$run.id <- as.character(sim_steup$run.id)

meta_df_prod <- meta_df_prod %>%
  dplyr::rename(run.id = V1) %>%
  dplyr::left_join(y = sim_steup, by = "run.id")

meta_unique <- unique(meta_df_prod[c(2:7, 12)])

# combine the four files
regions <- c(
  "total", "Vogtlandkreis",
  "Burgenlandkreis", "Greiz",
  "Altenburger Land"
)

landcover_variables <- c(211)
# landcover_variables <- c(1000, 211)

# used cores
ncores <- parallel::detectCores(logical = FALSE) / 3
print(ncores)


# loop over files
parallel::mclapply(1:nrow(meta_unique), function(igroup) {
  patterns_df <- meta_df_prod %>%
    dplyr::filter(V2 == meta_unique$V2[igroup]) %>%
    dplyr::filter(V3 == meta_unique$V3[igroup]) %>%
    dplyr::filter(V4 == meta_unique$V4[igroup]) %>%
    dplyr::filter(V5 == meta_unique$V5[igroup]) %>%
    dplyr::filter(V6 == meta_unique$V6[igroup]) %>%
    dplyr::filter(crop.id == meta_unique$crop.id[igroup])

  patterns_df <- apply(patterns_df[, 1:9], MARGIN = 1, function(x) {
    paste0(x, collapse = "_")
  })

  # get sub files
  sub_files <- unlist(lapply(patterns_df, FUN = function(x) {
    grep(x,
      nc.files,
      value = T
    )
  }))
  # get meta data
  sub_metadf <- as.data.frame(do.call(
    rbind,
    lapply(sub_files,
      FUN = str_split_custom
    )
  ))

  vars <- unique(sub_metadf$V2)
  ivar <- 1

  # get var-path
  sub_var <- unlist(stringr::str_split(sub_metadf$V2[1], "-"))
  var_path <- paste0(sub_var[1], "/", sub_var[1])

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

      tmp_file <- paste0(
        "input_BP_",
        igroup,
        unique(sub_metadf$V2),
        ilandcover,
        iregion
      )

      # exp_linear_trend(
      #   netCDF.files = sub_files,
      #   variable = unique(sub_metadf$V2),
      #   region = regions[iregion],
      #   landcover = landcover_variables[ilandcover],
      #   language = "DE",
      #   run_id = "6ter",
      #   output_path = output_folder,
      #   output_csv = NULL
      # )

      writeLines(text = c(
        sub_files,
        unique(sub_metadf$V2),
        regions[iregion],
        landcover_variables[ilandcover],
        # y.axis.limit,
        "DE",
        "6ter",
        output_folder,
        output_csv_folder
      ), tmp_file)

      # sub_exp_boxplots.R

      system(paste0(
        "Rscript --vanilla sub_exp_boxplots.R  ",
        tmp_file
      ))

      file.remove(tmp_file)
    }
  }
}, mc.cores = ncores)
# output_path<-"D:/AHomoudi/KlimaKonform/output/Alle/"
# ifile <- 1
# iregion <- 2
# ilandcover <- 1
# igroup <- 1
