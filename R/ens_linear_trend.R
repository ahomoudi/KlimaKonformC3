#' @title Plotting linear trend from  3 RCP scenario from ensemble files
#' @description A function that receives three netCDF files, and variable name,
#' statistical ensemble member and plot linear trend plot
#' the desired the out ṕut directory in which the output is placed.
#' @param netCDF.files The netCDF files that contain similar simulation variables.
#' They should be provided including the full path to the files
#' @param variable inside these netCDf files, e.g., AET
#' @param region A string referring to the region to plot; "total","Vogtlandkreis",
#' "Burgenlandkreis", "Greiz", and "Altenburger Land".
#' @param landcover A numeric variable indicating which land cover should be considered.
#' To consider all land cover give 1000.
#' @param stat_var A string referring to the statistical variable to plot in case the file is ensemble file,
#' for example, "mean","sd","median","max" and "min"
#' @param language A character variable either "EN" or "DE", to specify the languages of plots
#' @param output_path A string pointing to the output directory, it should end with "/"
#' @param output_csv A string pointing to the output directory for the csv file that is produced by
#' pre-processing netCDF files
#' @author Ahmed Homoudi
#' @return  PNG
#' @import stats
#' @importFrom utils globalVariables
#' @export
ens_linear_trend<- function(netCDF.files,
                            variable,
                            region,
                            landcover,
                            stat_var,
                            language,
                            output_path,
                            output_csv){


  # check files
  if(length(netCDF.files)!= 3) stop("not all RCPs files has been provided")

  # check Language
  if (language == "DE") {
    message("Producing Plots in German")

    utils::data("standard_output_de",
                envir = environment()
    )
  } else if (language == "EN") {
    message("Producing Plots in English")

    utils::data("standard_output_en",
                envir = environment()
    )
  } else {
    stop("Please select a language, either \"EN\" or \"DE\"")
  }

  # read files
  r.rast<-lapply(X = netCDF.files, FUN = terra::rast, subds = stat_var)

  # load spatial data -------------------------------------------------------

  # spatial data
  utils::data("Bundeslaender", envir = environment())
  utils::data("Landkreise", envir = environment())
  utils::data("land_cover", envir = environment())

  #STUIPED error
  #globalVariables("land_cover")

  # project shapefiles
  sf::st_crs(Landkreise) <- "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  sf::st_crs(Bundeslaender) <- "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

  # convert land cover to rast
  LC <- terra::rast(x = land_cover, type = "xyz")
  terra::crs(LC) <- terra::crs(r.rast[[1]])

  # terra::plot(LC)


  # get region
  # crop the data to the region
  if (region == "total") {
    message("Producing Plots considering the Whole model region")
  } else if (region != "total") {
    message(paste0("Producing Plots considering only: ", region))

    shp_lk <- Landkreise[Landkreise$Name == region, ]

    shp_ext <- sf::st_bbox(shp_lk)

    r.rast <- lapply(r.rast, FUN = terra::mask,
      mask = terra::vect(shp_lk))

    LC <- terra::mask(
      x = LC,
      mask = terra::vect(shp_lk)
    )
  } else {
    stop("Please specifiy a region to analyse! Either \"total\",\"Vogtlandkreis\", \n
         \"Burgenlandkreis\", \"Greiz\", or \"Altenburger Land\" ")
  }

  # obtain time series
  var_plotting<-as.Date(terra::time(r.rast[[1]]))

  r.mean<-lapply(r.rast, f1_mean)

  r.mean<-Reduce(cbind.data.frame, r.mean)

  colnames(r.mean)<-c("RCP2.6","RCP4.5","RCP8.5")

  var_plotting<-cbind(var_plotting, r.mean)
  # meta data
  # index of the variable in standard output
  variable_index <- which(standard_output_en$variable == variable)


  # plot


  # save plot


  # clean memory

}

# test

