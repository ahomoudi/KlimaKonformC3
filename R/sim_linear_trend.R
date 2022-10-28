#' @title Plotting linear trend from one simulation from KlimaKonform TB C3.
#' @description A function that receives one netCDF files, variable name,
#' and, custom y axis limits and plot linear trend plot.
#' The desired the out put directory in which the output is placed.
#' @param netCDF.file The netCDF file provided including the full path to the file
#' @param region A string referring to the region to plot; "total","Vogtlandkreis",
#' "Burgenlandkreis", "Greiz", and "Altenburger Land".
#' @param landcover A numeric variable indicating which land cover should be considered.
#' To consider all land cover give 1000.
#' @param y.axis.limits A vector of length 2L to produce all plots with similar y axis limits.
#' If not provided, each variable has being supplied with limits in the package.
#' @param language A character variable either "EN" or "DE", to specify the languages of plots
#' @param run_id A character variable either "2ter", "3ter", or "4ter" specificng the which run is used
#' to produce the ensemble.
#' @param output_path A string pointing to the output directory, it should end with "/"
#' @param output_csv A string pointing to the output directory for the csv file that is produced by
#' pre-processing netCDF file
#' @author Ahmed Homoudi
#' @return  PNG
#' @import stats
#' @importFrom utils globalVariables write.table
#' @export
sim_linear_trend <- function(netCDF.file,
                             region,
                             landcover,
                             y.axis.limits,
                             language,
                             run_id,
                             output_path,
                             output_csv) {


  # check files
  if (!file.exists(netCDF.file)) stop("The netCDF.file does not exist")

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
  r.rast <- terra::rast(netCDF.file)

  variable <- terra::varnames(r.rast)
  tmp <- unlist(stringr::str_split(netCDF.file, "/"))
  netCDF.file_withoutpath <- tmp[length(tmp)]

  # load spatial data -------------------------------------------------------

  # spatial data
  utils::data("Bundeslaender", envir = environment())
  utils::data("Landkreise", envir = environment())
  utils::data("land_cover", envir = environment())

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

    r.rast <- terra::mask(r.rast,
      mask = terra::vect(shp_lk)
    )

    LC <- terra::mask(
      x = LC,
      mask = terra::vect(shp_lk)
    )
  } else {
    stop("Please specifiy a region to analyse! Either \"total\",\"Vogtlandkreis\", \n
         \"Burgenlandkreis\", \"Greiz\", or \"Altenburger Land\" ")
  }

  # checking landcover ------------------------------------------------------

  # check landcover
  if (landcover == 1000) {
    message("Analysis will include all land cover types")

    # assign LC plot caption
    if (language == "DE") {
      LC_name <- paste0(enc2utf8("F\u00FCr"), " alle CORINE-Landbedeckungsklassen")
    } else if (language == "EN") {
      LC_name <- "For all CORINE Land Cover Classes"
    }
  } else {
    message(paste0("Analysis will include only landcover type: ", landcover))

    # for further analysis

    indices <- which(terra::values(LC) != landcover)

    # remove other LC classes
    r.rast <- f1_replace_values_in_SpatRaster_layers(r.rast, IDs = indices)

    # test
    # terra::plot(r.rast[[1]][[1]])

    # get the name of specific land cover
    utils::data("land_cover_legend", envir = environment())

    if (language == "DE") {
      LC_name <- paste0(
        " ", enc2utf8("F\u00FCr"), " CORINE-LB: ",
        land_cover_legend$LABEL3[which(land_cover_legend$CLC_CODE == landcover)]
      )
    } else if (language == "EN") {
      LC_name <- LC_name <- paste0(
        "For CORINE-LC: ",
        land_cover_legend$LABEL3[which(land_cover_legend$CLC_CODE == landcover)]
      )
    }
  }

  # prepare timeseries  -----------------------------------------------------

  # obtain time series
  var_plotting <- data.frame(YEAR = as.Date(terra::time(r.rast)))

  var_plotting$Kmean <- unlist(terra::global(r.rast, "mean", na.rm = T))
  var_plotting$Kmax <- unlist(terra::global(r.rast, "max", na.rm = T))
  var_plotting$Kmin <- unlist(terra::global(r.rast, "min", na.rm = T))

  gc(verbose = F)

  # meta data ---------------------------------------------------------------

  # index of the variable in standard output
  variable_index <- which(standard_output_en$variable == variable)

  if (language == "DE") {
    var_units <- standard_output_de$units[variable_index]
    var_name <- standard_output_de$longname[variable_index]

    if (region == "total") {
      plot.title <- paste0(var_name, " ", enc2utf8("f\u00FCr"), " die Gesamtmodellregion")
    } else {
      plot.title <- paste0(var_name, " f\u00fcr ", region)
    }
    plot.caption <- paste(paste0(
      "\u00a9 KlimaKonform ", lubridate::year(Sys.time()),
      ". ",
      LC_name
    ),
    paste0("Quelle: ", netCDF.file_withoutpath),
    sep = "\n"
    )
  } else if (language == "EN") {
    var_units <- standard_output_en$units[variable_index]
    var_name <- standard_output_en$longname[variable_index]

    if (region == "total") {
      plot.title <- paste0(var_name, " for the Total Model Region")
    } else {
      plot.title <- paste0(var_name, " for ", region)
    }
    plot.caption <- paste(paste0(
      "\u00a9 KlimaKonform ", lubridate::year(Sys.time()),
      ". ",
      LC_name
    ),
    paste0("Source: ", netCDF.file_withoutpath),
    sep = "\n"
    )
  }


  # plots and csv name  -----------------------------------------------------

  # define plot name
  if (exists("output_path")) {
    plot_name <- paste0(
      output_path,
      gsub(".nc", "_LT_.png", x = netCDF.file_withoutpath)
    )
  } else {
    plot_name <- gsub(".nc", "_LT_.png", x = netCDF.file_withoutpath)
  }


  # define csv name
  if (exists("output_path")) {
    csv_name <- paste0(
      output_path,
      gsub(".nc", "_LT_.csv.gz", x = netCDF.file_withoutpath)
    )
  } else {
    csv_name <- gsub(".nc", "_LT_.csv.gz", x = netCDF.file_withoutpath)
  }

  # pre-plot --------------------------------------------------------------------
  # read logo
  # KlimaKonform_img <- project_logo()
  # grid::rasterGrob(KlimaKonform_img)

  # time axis

  if (!exists("y.axis.limits")) {
    csv_file <- system.file("2ter_lauf.csv",
      package = "KlimaKonformC3"
    )

    csv_file <- readr::read_csv(csv_file, show_col_types = F)

    csv_file <- csv_file[grep(x = csv_file$variable, pattern = variable), ]

    y.axis.max <- max(csv_file$maximum, na.rm = T)
    y.axis.min <- min(csv_file$minimum, na.rm = T)

    if (y.axis.max - y.axis.min > 100) {
      # round up/down to next 50
      y.axis.max <- round_custom(y.axis.max, 50, 1)
      y.axis.min <- round_custom(y.axis.min, 50, 0)
    } else if (y.axis.max - y.axis.min < 100 & y.axis.max - y.axis.min > 50) {
      # round up/down to next 10
      y.axis.max <- round_custom(y.axis.max, 10, 1)
      y.axis.min <- round_custom(y.axis.min, 10, 0)
    } else {
      # round to next 5
      # round up/down to next 50
      y.axis.max <- round_custom(y.axis.max, 5, 1)
      y.axis.min <- round_custom(y.axis.min, 5, 0)
    }

    y.axis.limits <- c(y.axis.min, y.axis.max)
  }

  # colors

  if (stringr::str_detect("rcp26", string = netCDF.file_withoutpath)) {
    rcps_colours <- c("#003466")
  } else if (stringr::str_detect("rcp45", string = netCDF.file_withoutpath)) {
    rcps_colours <- c("#70A0CD")
  } else if (stringr::str_detect("rcp85", string = netCDF.file_withoutpath)) {
    rcps_colours <- c("#990002")
  } else {
    stop("This file doesn't belong to any rcp Sceniro")
  }

  # grid::grid.raster( rcps_colours , interpolate = FALSE)


  # plot --------------------------------------------------------------------

  figure <- ggplot2::ggplot(var_plotting) +

    # ribbon
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(
        x = YEAR,
        ymin = Kmin,
        ymax = Kmax,
        fill = rcps_colours
      ),
      alpha = 0.25
    ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(x = YEAR, y = Kmean, color = rcps_colours),
      size = 0.3,
      show.legend = T
    ) +
    # mean trend line
    ggplot2::geom_smooth(
      mapping = ggplot2::aes(x = YEAR, y = Kmean, color = rcps_colours),
      method = "lm",
      linetype = "longdash",
      size = 0.3,
      se = F
    ) +
    ggplot2::scale_color_manual(values = rcps_colours) +
    ggplot2::scale_fill_manual(values = rcps_colours) +
    ggplot2::theme_bw(base_size = 6) +

    # add axis title
    ggplot2::xlab("") +
    ggplot2::ylab(paste0(
      var_name,
      " [", var_units, "]"
    )) +

    # set axis breaks
    ggplot2::scale_y_continuous(
      limits = y.axis.limits,
      expand = c(0, 0)
    ) +
    ggplot2::scale_x_date(
      date_breaks = "10 years",
      minor_breaks = "5 years",
      expand = c(0, 0),
      date_labels = "%Y"
    ) +
    ggplot2::labs(
      title = plot.title,
      caption = plot.caption,
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        size = 6
      ),
      axis.title.y = ggplot2::element_text(
        hjust = 0.5,
        size = 5,
        margin = ggplot2::margin(0, 5, 0, 0)
      ),
      axis.text = ggplot2::element_text(
        hjust = 0.5,
        size = 5,
        colour = "black"
      ),
      plot.caption = ggplot2::element_text(
        hjust = c(0),
        size = 4,
        colour = "blue",
        margin = ggplot2::margin(0, 0, 0, 0)
      )
    ) +

    # set legend
    ggplot2::theme(
      legend.position = "none"
    )

  # # add logo
  # ggplot2::annotation_custom(KlimaKonform_img
  # )

  # post-plot ---------------------------------------------------------------

  ggplot2::ggsave(
    plot = figure,
    filename = plot_name,
    units = "mm",
    width = 150,
    height = 80,
    dpi = 300,
    device = "png"
  )

  # write csv to disk
  crunch::write.csv.gz(
    x = var_plotting %>%
      dplyr::mutate_if(is.numeric, round, digits = 3),
    quote = FALSE,
    file = csv_name,
    row.names = F
  )

  # End ---------------------------------------------------------------------
  # clean
  rm(list = ls())
  gc()
}
