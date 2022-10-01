#' @title Plotting 12 maps (3 historical + 9 relative change) from 3 RCP scenarios
#' @description A function that receives three netCDF files, and variable name,
#' statistical ensemble member and plot 12 maps, considering 3 RCPs X four periods
#' it in the desired the out put directory in which the output is placed.
#' the first three maps represents the mean of the historical period, and the rest
#' represents the relative change to the historical period
#' @param netCDF.files The netCDF files that contain similar simulation
#' variables. They should be provided including the full path to the files
#' @param variable inside these netCDf files, e.g., AET
#' @param region A string referring to the region to plot; "total",
#' "Vogtlandkreis", "Burgenlandkreis", "Greiz", and "Altenburger Land".
#' @param landcover A numeric variable indicating which land cover should be
#' considered. To consider all land cover give 1000.
#' @param stat_var A string referring to the statistical variable to plot in
#' case the file is ensemble file, for example, "mean","sd","median","max"
#' and "min"
#' @param language A character variable either "EN" or "DE", to specify the
#' languages of plots
#' @param run_id A character variable either "2ter", "3ter", or "4ter"
#' specifying the which run is used to produce the ensemble.
#' @param output_path A string pointing to the output directory,
#'  it should end with "/"
#' @param output_csv A string pointing to the output directory for the csv
#' file that is produced by pre-processing netCDF files
#' @author Ahmed Homoudi
#' @return  PNG
#' @import stats
#' @importFrom utils write.table
#' @importFrom grDevices rgb
#' @export
ens_12maps_relative_change <- function(netCDF.files,
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

    standard_output<-standard_output_de

  } else if (language == "EN") {
    message("Producing Plots in English")

    standard_output<-standard_output_en
  } else {
    stop("Please select a language, either \"EN\" or \"DE\"")
  }

  netCDF.files <- sort(netCDF.files, decreasing = F)
  # read files
  r.rast <- lapply(X = netCDF.files, FUN = terra::rast, subds = stat_var)

  names(r.rast) <- c("RCP2.6", "RCP4.5", "RCP8.5")

  # load spatial data -------------------------------------------------------

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

    shp_lk <- Landkreise
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

  # prepare maps data-----------------------------------------------------

  var_plotting <- lapply(r.rast, relative_change_four_periods)

  # https://stackoverflow.com/a/53969052/13818750

  var_plotting <- var_plotting %>%
    purrr::imap(.x = ., ~ purrr::set_names(.x, c("x", "y", "Periods", .y))) %>%
    purrr::reduce(dplyr::left_join, by = c("x", "y", "Periods")) %>%
    tidyr::pivot_longer(
      cols = -c("x", "y", "Periods"),
      names_to = "Scenario",
      values_to = "Kvalue"
    )

  # replace periods with actual years
  var_plotting$Periods <- ifelse(var_plotting$Periods == "period1",
    "1971-2000",
    ifelse(var_plotting$Periods == "period2",
      "1991-2020",
      ifelse(var_plotting$Periods == "period3",
        "2021-2050",
        ifelse(var_plotting$Periods == "period4",
          "2070-2099",
          NA
        )
      )
    )
  )

  # clean some memory
  gc(verbose = F)

  # meta data ---------------------------------------------------------------

  # index of the variable in standard output
  variable_index <- which(standard_output$variable == variable)

  if (language == "DE") {
    var_units <- standard_output$units[variable_index]
    var_name <- standard_output$longname[variable_index]

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
    var_units <- standard_output$units[variable_index]
    var_name <- standard_output$longname[variable_index]

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
    # legend.title <- "Szenario"
    legend.title1 <- paste0(var_name, " [", var_units, "]")
    legend.title2 <- paste0("Veraenderung [", var_units, "]")
  }

  if (language == "EN") {
    # legend.title <- "Scenario"
    legend.title1 <- paste0(var_name, " [", var_units, "]")
    legend.title2 <- paste0("Change [", var_units, "]")
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
      "3XRCPs_12maps_relCh_.png"
    )
  } else {
    plot_name <- paste0(
      variable, "_",
      "ensemble_",
      run_id, "_lauf_",
      "3XRCPs_12maps_relCh_.png"
    )
  }
  # define csv name
  if (exists("output_csv")) {
    csv_name <- paste0(
      output_csv,
      variable, "_",
      "ensemble_",
      run_id, "_lauf_",
      "3XRCPs_12maps_relCh_.csv.gz"
    )
  } else {
    csv_name <- paste0(
      variable, "_",
      "ensemble_",
      run_id, "_lauf_",
      "3XRCPs_12maps_relCh_.csv.gz"
    )
  }

  colnames(var_plotting) <- c("x", "y", "Period", "Scenario", "Kvalue")

  # pre-plot --------------------------------------------------------------------
  # read logo
  # KlimaKonform_img <- project_logo()
  # grid::rasterGrob(KlimaKonform_img)

  # y axis

  # fix limts for historical
  y.axis.max_hist <- max(var_plotting$Kvalue[var_plotting$Period == "1971-2000"],
    na.rm = T
  )
  y.axis.min_hist <- min(var_plotting$Kvalue[var_plotting$Period == "1971-2000"],
    na.rm = T
  )
  y.limits <- setting_nice_limits(y.axis.min_hist, y.axis.max_hist)

  y.axis.max_hist <- y.limits[2]
  y.axis.min_hist <- y.limits[1]

  # fix limits for the relative change
  y.axis.max_relCh <- max(var_plotting$Kvalue[var_plotting$Period != "1971-2000"],
    na.rm = T
  )
  y.axis.min_relCh <- min(var_plotting$Kvalue[var_plotting$Period != "1971-2000"],
    na.rm = T
  )
  y.limits <- setting_nice_limits(y.axis.min_relCh, y.axis.max_relCh)

  y.axis.max_relCh <- y.limits[2]
  y.axis.min_relCh <- y.limits[1]

  shp_lk <- sf::st_as_sf(shp_lk)
  Bundeslaender <- sf::st_as_sf(Bundeslaender)

  # get extent
  if (region == "total") {
    xlim_sf <- c(min(var_plotting$x, na.rm = T), max(var_plotting$x, na.rm = T))
    ylim_sf <- c(min(var_plotting$y, na.rm = T), max(var_plotting$y, na.rm = T))
  } else {
    xlim_sf <- sf::st_bbox(shp_lk)[c(1, 3)]
    ylim_sf <- sf::st_bbox(shp_lk)[c(2, 4)]
  }

  # scales::show_col(rcps_colours_temp(50))
  # scales::show_col(rcps_colours_precip(50))
  # plot --------------------------------------------------------------------

  figure <- ggplot2::ggplot() +
    ggplot2::geom_tile(
      data = var_plotting %>%
        dplyr::filter(Period == "1971-2000"),
      ggplot2::aes(x, y, fill = Kvalue)
    ) +
    ggplot2::scale_fill_gradientn(
      colors = rcps_colours_temp(50),
      na.value = "grey77",
      limits = c(y.axis.min_hist, y.axis.max_hist),
      expand = c(0, 0),
      name = legend.title1,
      guide = ggplot2::guide_colourbar(
        title.position = "bottom"
      )
    ) +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_tile(
      data = var_plotting %>%
        dplyr::filter(Period != "1971-2000"),
      ggplot2::aes(x, y, fill = Kvalue)
    ) +
    ggplot2::scale_fill_gradientn(
      colors = rcps_colours_precip(50),
      na.value = "grey77",
      limits = c(y.axis.min_relCh, y.axis.max_relCh),
      expand = c(0, 0),
      name = legend.title2,
      guide = ggplot2::guide_colourbar(
        title.position = "bottom"
      )
    ) +
    ggplot2::facet_grid(Period ~ Scenario) +
    ggplot2::geom_sf(
      data = Bundeslaender,
      mapping = ggplot2::aes(),
      fill = NA,
      size = 0.5,
      color = "grey31"
    ) +
    ggplot2::geom_sf(
      data = shp_lk,
      mapping = ggplot2::aes(),
      fill = NA,
      size = 0.3,
      color = "black"
    ) +
    ggplot2::coord_sf(
      xlim = xlim_sf,
      ylim = ylim_sf,
      expand = FALSE
    ) +
    ggplot2::theme_bw(base_size = 8) +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::labs(
      title = plot.title,
      caption = plot.caption,
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        size = 8,
        margin = ggplot2::margin(2, 0, 2, 0, "mm")
      ),

      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),

      plot.caption = ggplot2::element_text(
        hjust = c(0),
        size = 6,
        colour = "blue",
        margin = ggplot2::margin(2, 0, 2, 0, "mm")
      )
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.width = ggplot2::unit(10, "mm"),
      legend.key.height = ggplot2::unit(2, "mm"),
      legend.text = ggplot2::element_text(
        colour = "black",
        size = 5,
        family = "sans"
      ),
      legend.title = ggplot2::element_text(
        colour = "black",
        size = 6,
        family = "sans"
      ),
      legend.title.align = 0.5
    )+
    # format plot background
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "grey77"),
      plot.margin = ggplot2::margin(0, 0, 0, 0, "mm")
    )

  # post-plot ---------------------------------------------------------------

  ggplot2::ggsave(
    plot = figure,
    filename = plot_name,
    units = "mm",
    width = 120,
    height = 150,
    dpi = 300,
    device = "png"
  )

  # write csv to disk

  crunch::write.csv.gz(
    x = na.omit(var_plotting) %>%
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
