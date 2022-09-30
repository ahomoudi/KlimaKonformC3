#' @title Plotting linear trend from  3 RCP scenario from ensemble files
#' @description A function that receives three netCDF files, and variable name,
#' statistical ensemble member and plot linear trend plot
#' the desired the out put directory in which the output is placed.
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
#' @param run_id A character variable either "2ter", "3ter", or "4ter" specificng the which run is used
#' to produce the ensemble.
#' @param output_path A string pointing to the output directory, it should end with "/"
#' @param output_csv A string pointing to the output directory for the csv file that is produced by
#' pre-processing netCDF files
#' @author Ahmed Homoudi
#' @return  PNG
#' @import stats
#' @importFrom utils globalVariables write.table
#' @export
ens_linear_trend <- function(netCDF.files,
                             variable,
                             region,
                             landcover,
                             stat_var,
                             language,
                             run_id,
                             output_path,
                             output_csv) {


  # check files
  if (length(netCDF.files) != 3) stop("not all RCPs files has been provided")

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

  netCDF.files <- sort(netCDF.files, decreasing = F)
  # read files
  r.rast <- lapply(X = netCDF.files, FUN = terra::rast, subds = stat_var)

  names(r.rast) <- c("RCP2.6", "RCP4.5", "RCP8.5")

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

    r.rast <- lapply(r.rast,
      FUN = terra::mask,
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
    r.rast <- lapply(r.rast, f1_replace_values_in_SpatRaster_layers, IDs = indices)

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
  var_plotting <- as.Date(terra::time(r.rast[[1]]))

  r.mean <- lapply(r.rast, f1_mean)
  r.mean <- Reduce(cbind.data.frame, r.mean)
  colnames(r.mean) <- c("RCP2.6", "RCP4.5", "RCP8.5")

  var_plotting <- cbind(YEAR = var_plotting, r.mean)

  var_plotting <- var_plotting %>%
    tidyr::pivot_longer(
      cols = 2:4,
      names_to = "Scenario",
      values_to = "Kmean"
    )


  rm(r.mean)
  gc(verbose = F)

  # max
  r.max <- lapply(r.rast, f1_max)
  r.max <- Reduce(cbind.data.frame, r.max)
  # use from different files to get consistency
  r.max <- cbind(YEAR = as.Date(terra::time(r.rast[[2]])), r.max)
  colnames(r.max) <- c("YEAR", "RCP2.6", "RCP4.5", "RCP8.5")

  var_plotting <- r.max %>%
    tidyr::pivot_longer(
      cols = 2:4,
      names_to = "Scenario",
      values_to = "Kmax"
    ) %>%
    dplyr::right_join(y = var_plotting, by = c("YEAR", "Scenario"))

  rm(r.max)
  gc(verbose = F)

  # min
  r.min <- lapply(r.rast, f1_min)
  r.min <- Reduce(cbind.data.frame, r.min)
  # use from different files to get consistency
  r.min <- cbind(YEAR = as.Date(terra::time(r.rast[[3]])), r.min)
  colnames(r.min) <- c("YEAR", "RCP2.6", "RCP4.5", "RCP8.5")

  var_plotting <- r.min %>%
    tidyr::pivot_longer(
      cols = 2:4,
      names_to = "Scenario",
      values_to = "Kmin"
    ) %>%
    dplyr::right_join(y = var_plotting, by = c("YEAR", "Scenario"))


  rm(r.min)
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
    paste0("Quelle: Ensemble no. ", run_id),
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
    paste0("Source: Ensemble no.", run_id),
    sep = "\n"
    )
  }


  # change name of stat_var -------------------------------------------------

  # english,"mean","sd","median","max" and "min"
  if (language == "EN") {
    if (stat_var == "mean") {
      plot.title <- paste0(
        plot.title,
        "\n (Mean Ensemble)"
      )
    }
    if (stat_var == "sd") {
      plot.title <- paste0(
        plot.title,
        "\n ( Standard deviation Ensemble)"
      )
    }
    if (stat_var == "max") {
      plot.title <- paste0(
        plot.title,
        "\n ( Maximum Ensemble)"
      )
    }
    if (stat_var == "min") {
      plot.title <- paste0(
        plot.title,
        "\n (Minimum Ensemble)"
      )
    }
    if (stat_var == "median") {
      plot.title <- paste0(
        plot.title,
        "\n (Median Ensemble)"
      )
    }

    # deutsch
  } else if (language == "DE") {
    if (stat_var == "mean") {
      plot.title <- paste0(
        plot.title,
        "\n (Mittelwert Ensemble)"
      )
    }
    if (stat_var == "sd") {
      plot.title <- paste0(
        plot.title,
        "\n (Standardabweichung Ensemble)"
      )
    }
    if (stat_var == "max") {
      plot.title <- paste0(
        plot.title,
        "\n (Maximum Ensemble)"
      )
    }
    if (stat_var == "min") {
      plot.title <- paste0(
        plot.title,
        "\n (Minimum Ensemble)"
      )
    }
    if (stat_var == "median") {
      plot.title <- paste0(
        plot.title,
        "\n (Zentralwert Ensemble)"
      )
    }
  }

  if (language == "DE") {
    legend.title <- "Szenario"
  }

  if (language == "EN") {
    legend.title <- "Scenario"
  }

  #
  # plots and csv name  -----------------------------------------------------

  # define plot name
  if (exists("output_path")) {
    plot_name <- paste0(
      output_path,
      variable, "_",
      "ensemble_",
      run_id, "_lauf_",
      "3XRCPs_LT_.png"
    )
  } else {
    plot_name <- paste0(
      variable, "_",
      "ensemble_",
      run_id, "_lauf_",
      "3XRCPs_LT_.png"
    )
  }
  # define csv name
  if (exists("output_csv")) {
    csv_name <- paste0(
      output_csv,
      variable, "_",
      "ensemble_",
      run_id, "_lauf_",
      "3XRCPs_LT_.csv.gz"
    )
  } else {
    csv_name <- paste0(
      variable, "_",
      "ensemble_",
      run_id, "_lauf_",
      "3XRCPs_LT_.csv.gz"
    )
  }

  # pre-plot --------------------------------------------------------------------
  # read logo
  # KlimaKonform_img <- project_logo()
  # grid::rasterGrob(KlimaKonform_img)

  # time axis
  x.axis <- unique(var_plotting$YEAR)

  drops <- c("YEAR", "Scenario")

  y.axis.max <- max(var_plotting[, !(names(var_plotting) %in% drops)], na.rm = T)
  y.axis.min <- min(var_plotting[, !(names(var_plotting) %in% drops)], na.rm = T)

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

  # y.axis.interval<-setting_nice_intervals(minval = y.axis.min,
  #                        maxval = y.axis.max)


  # colors
  rcps_colours <- c("#003466", "#70A0CD", "#990002")


  # plot --------------------------------------------------------------------

  figure <- ggplot2::ggplot(var_plotting) +

    # ribbon
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(
        x = YEAR,
        ymin = Kmin,
        ymax = Kmax,
        fill = Scenario
      ),
      alpha = 0.25
    ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(x = YEAR, y = Kmean, color = Scenario),
      size = 0.3,
      show.legend = T
    ) +
    # mean trend line
    ggplot2::geom_smooth(
      mapping = ggplot2::aes(x = YEAR, y = Kmean, color = Scenario),
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
      limits = c(y.axis.min, y.axis.max),
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
      legend.title = ggplot2::element_blank(),
      legend.key.size = ggplot2::unit(4, "mm"),
      legend.margin = ggplot2::margin(0, 0, 0, 0, unit = "mm"),
      legend.text = ggplot2::element_text(size = 5)
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
