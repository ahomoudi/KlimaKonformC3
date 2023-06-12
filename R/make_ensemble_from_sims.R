#' @title Making an ensemble from individual simulations (i.e. individual simulation as netCDf files)
#'
#' @description A function that receives multiple netCDF files, and variable name,
#' the desired the output directory in which the output is placed.
#'
#' @param nc.files The netCDF files that contain similar simulation variables.
#' They should be provided including the full path to the files
#' @param variable inside these netCDf files, e.g., AET
#' @param output_path A string pointing to the output directory, it should end with "/"
#' @author Ahmed Homoudi
#' @return  netCDf file
#' @import stats
#' @export

make_ensemble_from_sims <- name <- function(nc.files,
                                            variable,
                                            output_path) {
  # first get meta data
  # meta data
  str_split_custom <- function(X) {
    first <- unlist(stringr::str_split(X, pattern = "/"))

    Xr <- unlist(stringr::str_split(first[length(first)], pattern = "_"))

    Xr[length(Xr)] <- unlist(stringr::str_split(Xr[length(Xr)], pattern = "[.]"))[1]

    return(Xr)
  }

  # convert NetCDF files to metada dataframe
  meta_df <- as.data.frame(do.call(rbind, lapply(nc.files, FUN = str_split_custom)))

  # read all netCDF files in a list
  ras.files <- lapply(nc.files, terra::rast)

  # get numbers of cores
  ncores <- parallel::detectCores(all.tests = FALSE, logical = TRUE) - 1

  # convert to SpatRasterDataset
  s <- terra::sds(ras.files)

  # calculate different ensemble statistics

  # https://stackoverflow.com/a/73777020/13818750
  Kmean <- terra::app(s, mean,
    cores = ncores
  )

  Kmedian <- terra::app(s, median,
    cores = ncores
  )
  Ksd <- terra::app(s, sd,
    cores = ncores
  )
  KMin <- terra::app(s, min,
    cores = ncores
  )
  KMax <- terra::app(s, max,
    cores = ncores
  )

  # Create a SpatRasterDataset
  all_stats <- terra::sds(Kmean, Ksd, Kmedian, KMax, KMin)

  utils::data("standard_output_en", envir = environment())

  # index of the variable in standard output
  variable_index <- which(standard_output_en$variable == variable)
  # assign units
  terra::units(all_stats) <- rep(standard_output_en$units[variable_index], 5)

  # assign short name
  terra::varnames(all_stats) <- c("mean", "sd", "median", "max", "min")

  # assign long name
  terra::longnames(all_stats) <- rep(standard_output_en$longname[variable_index], 5)


  # Writing netcdf file of the cropped SpatRaster

  # check time
  if (length(unique(meta_df$V9)) > 1) {
    end_date <- unique(meta_df$V9)[1]
  } else {
    end_date <- unique(meta_df$V9)
  }

  if (length(unique(meta_df$V8)) > 1) {
    start_date <- unique(meta_df$V8)[1]
  } else {
    start_date <- unique(meta_df$V8)
  }

  ncname <- paste0(
    unique(meta_df$V5), "_",
    unique(meta_df$V2), "_ensemble-stats_",
    start_date, "_",
    end_date, ".nc"
  )

  # netCDF file location
  ncfname <- paste0(output_path, ncname)

  terra::writeCDF(
    x = all_stats,
    filename = ncfname,
    compression = 9,
    missval = 1e32,
    zname = "time",
    overwrite = TRUE
  )

  # clean
  rm(list = ls())
  invisible(gc())
  message("Done ;), I also cleaned the memory for you ;)")
}


# test
# nc.files <- c(
#   "/media/ahmed/Daten/WHK2/Data/netCDF/3ter_lauf/rcp26/260109_AET_MOHC-HadGEM2-ES_KNMI-RACMO22E_rcp26_r1i1p1_v2_1970-01-01_2099-12-31.nc",
#   "/media/ahmed/Daten/WHK2/Data/netCDF/3ter_lauf/rcp26/260110_AET_MOHC-HadGEM2-ES_KNMI-RACMO22E_rcp26_r1i1p1_v2_1970-01-01_2099-12-31.nc"
# )
# variable <- "AET"
# output_path <- "/media/ahmed/Daten/WHK2/Data/netCDF/3ter_lauf/"
