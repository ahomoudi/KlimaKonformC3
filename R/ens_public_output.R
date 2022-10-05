#' @title Plotting one informative plot containing, relative change maps,
#' boxplots and time series.
#' @description A function that receives one netCDF files, and variable name,
#' statistical ensemble member and plot 4 maps, boxplots, & timeseries considering one RCPs.
#' it in the desired the out put directory in which the output is placed.
#' the first three maps represents the mean of the historical period, and the rest
#' represents the relative change to the historical period
#' @param netCDF.file The netCDF file that contain similar simulation
#' variables. It should be provided including the full path to the files
#' @param variable inside these netCDf file, e.g., AET
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
ens_public_output <- function(netCDF.file,
                              variable,
                              region,
                              landcover,
                              stat_var,
                              language,
                              run_id,
                              output_path,
                              output_csv) {


  # check files
  if (length(netCDF.file) != 1) stop("not all RCPs files has been provided")

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
  r.rast <- terra::rast(netCDF.file, subds = stat_var)

  # load spatial data -------------------------------------------------------

  utils::data("Bundeslaender",
    envir = environment()
  )
  utils::data("Landkreise",
    envir = environment()
  )
  utils::data("land_cover",
    envir = environment()
  )

  # project shapefiles
  sf::st_crs(Landkreise) <- "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  sf::st_crs(Bundeslaender) <- "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

  # convert land cover to rast
  LC <- terra::rast(x = land_cover, type = "xyz")
  terra::crs(LC) <- terra::crs(r.rast)

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

    r.rast <- terra::mask(r.rast, mask = terra::vect(shp_lk))

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
    r.rast <- f1_replace_values_in_SpatRaster_layers(r.rast,
      IDs = indices
    )

    # test
    # terra::plot(r.rast[[1]][[1]])
    utils::data("land_cover_legend",
      envir = environment()
    )
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

  var_plotting_maps <- relative_change_four_periods(r.rast)

  # replace periods with actual years
  var_plotting_maps$Periods <- ifelse(var_plotting_maps$Periods == "period1",
    "1971-2000",
    ifelse(var_plotting_maps$Periods == "period2",
      "1991-2020",
      ifelse(var_plotting_maps$Periods == "period3",
        "2021-2050",
        ifelse(var_plotting_maps$Periods == "period4",
          "2070-2099",
          NA
        )
      )
    )
  )

  colnames(var_plotting_maps) <- c("x", "y", "Period", "Kvalue")
  # clean some memory
  gc(verbose = F)

  # prepare boxplots data-----------------------------------------------------

  var_plotting_bp <- boxplot_df(r.rast)

  gc(verbose = F)

  # prepare boxplots data-----------------------------------------------------

  var_plotting_lt <- linear_trend_df(r.rast)

  gc(verbose = F)

  # meta data ---------------------------------------------------------------

  # file name without path
  netCDF.file <- unlist(stringr::str_split(netCDF.file, "/"))[length(
    unlist(stringr::str_split(netCDF.file, "/"))
  )]

  netCDF.file <- stringr::str_remove(netCDF.file, ".nc")

  if (language == "DE") {
    # index of the variable in standard output
    variable_index <- which(standard_output_de$variable == variable)
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
    paste0("Quelle: ", netCDF.file),
    sep = "\n"
    )
  } else if (language == "EN") {
    # index of the variable in standard output
    variable_index <- which(standard_output_en$variable == variable)
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
    paste0("Source: ", netCDF.file),
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
      netCDF.file,
      "_public_output_.png"
    )
  } else {
    plot_name <- paste0(
      netCDF.file,
      "_public_output_.png"
    )
  }
  # define csv name
  if (exists("output_csv")) {
    csv_name <- paste0(
      output_csv,
      variable, "_",
      "ensemble_",
      run_id, "_lauf_",
      "3XRCPs_12maps_public_output_"
    )
  } else {
    csv_name <- paste0(
      variable, "_",
      "ensemble_",
      run_id, "_lauf_",
      "3XRCPs_12maps_public_output_"
    )
  }


  # pre-plot --------------------------------------------------------------------
  # read logo
  # KlimaKonform_img <- project_logo()
  # grid::rasterGrob(KlimaKonform_img)

  # y axis

  # fix limts for historical
  y.axis.max_hist <- max(var_plotting_maps$Kvalue[var_plotting_maps$Period == "1971-2000"],
    na.rm = T
  )
  y.axis.min_hist <- min(var_plotting_maps$Kvalue[var_plotting_maps$Period == "1971-2000"],
    na.rm = T
  )
  y.limits <- setting_nice_limits(y.axis.min_hist, y.axis.max_hist)

  y.axis.max_hist <- y.limits[2]
  y.axis.min_hist <- y.limits[1]

  # fix limits for the relative change
  y.axis.max_relCh <- max(var_plotting_maps$Kvalue[var_plotting_maps$Period != "1971-2000"],
    na.rm = T
  )
  y.axis.min_relCh <- min(var_plotting_maps$Kvalue[var_plotting_maps$Period != "1971-2000"],
    na.rm = T
  )
  y.limits <- setting_nice_limits(y.axis.min_relCh, y.axis.max_relCh)

  y.axis.max_relCh <- y.limits[2]
  y.axis.min_relCh <- y.limits[1]

  shp_lk <- sf::st_as_sf(shp_lk)
  Bundeslaender <- sf::st_as_sf(Bundeslaender)


  # limits of boxplots and timeseies

  y.axis.max <- max(
    max(var_plotting_lt[, -1], na.rm = T),
    max(var_plotting_bp$Kvalue, na.rm = T)
  )

  y.axis.min <- min(
    min(var_plotting_lt[, -1], na.rm = T),
    min(var_plotting_bp$Kvalue, na.rm = T)
  )

  y.limits <- setting_nice_limits(y.axis.min, y.axis.max)

  y.axis.max <- y.limits[2]
  y.axis.min <- y.limits[1]

  # get extent
  if (region == "total") {
    xlim_sf <- c(min(var_plotting_maps$x, na.rm = T), max(var_plotting_maps$x, na.rm = T))
    ylim_sf <- c(min(var_plotting_maps$y, na.rm = T), max(var_plotting_maps$y, na.rm = T))
  } else {
    xlim_sf <- sf::st_bbox(shp_lk)[c(1, 3)]
    ylim_sf <- sf::st_bbox(shp_lk)[c(2, 4)]
  }

  # scales::show_col(rcps_colours_temp(50))
  # scales::show_col(rcps_colours_precip(50))
  # plot --------------------------------------------------------------------

  fig_a <- ggplot2::ggplot() +
    ggplot2::geom_tile(
      data = var_plotting_maps %>%
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
        title.position = "bottom",
        order = 1
      )
    ) +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_tile(
      data = var_plotting_maps %>%
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
        title.position = "bottom",
        order = 2
      )
    ) +
    ggplot2::facet_wrap(~Period, nrow = 1) +
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
    ) +
    # format plot background
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "grey77"),
      plot.margin = ggplot2::margin(rep(1.25,4), "mm"),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )

  # colors
  rcps_colours <- c("#003466", "#70A0CD", "#990002")

  fig_b <- ggplot2::ggplot(
    data = var_plotting_bp,
    mapping = ggplot2::aes(
      x = Period,
      y = Kvalue
    )
  ) +
    # ggplot2::geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+

    ggplot2::geom_boxplot(
      outlier.colour = "red", outlier.shape = 4,
      outlier.size = 0.1, outlier.alpha = 0.5,
      fatten = 0.3, size = 0.3
    ) +
    ggplot2::stat_boxplot(
      geom = "errorbar",
      size = 0.3
    ) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::theme_bw(base_size = 8) +

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
    ggplot2::theme(
      legend.position = "none",
      plot.margin = ggplot2::margin(rep(1.25,4), "mm"),
      axis.title.y = ggplot2::element_text(
        hjust = 0.5,
        size = 5,
        margin = ggplot2::margin(0, 5, 0, 0)
      ),
      axis.text = ggplot2::element_text(
        hjust = 0.5,
        size = 5,
        colour = "black"
      )
    )


  fig_c <- ggplot2::ggplot(var_plotting_lt) +

    # ribbon
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(
        x = YEAR,
        ymin = Kmin,
        ymax = Kmax
      ),
      alpha = 0.25
    ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(x = YEAR, y = Kmean),
      size = 0.3,
      show.legend = T
    ) +
    # mean trend line
    ggplot2::geom_smooth(
      mapping = ggplot2::aes(x = YEAR, y = Kmean),
      method = "lm",
      linetype = "longdash",
      size = 0.3,
      se = F
    ) +
    ggplot2::theme_bw(base_size = 8) +

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
    ggplot2::theme(
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
      plot.margin = ggplot2::margin(1.25, 1.25, -2, 1.25, "mm")
    )

  # create title for plot
  title <- cowplot::ggdraw() +
    cowplot::draw_label(
      plot.title,
      #fontface = 'bold',
      x = 0.5,
      hjust = 0.5,
      y = 0.5,
      vjust = 0.5,
      size = 8,
    )

  figure <-cowplot::plot_grid(
    title,
    fig_a,
    fig_b,
    fig_c,
    ncol = 1,
    nrow = 4,
    axis = 'A',
    align = "v",
    labels = c(
               NA,'A', 'B', 'C'
               ),
    label_size = 8,
    label_x = 0,
    label_y = 1.1,
    rel_heights = c(0.15,1.2,0.85,0.85))


  figure <-cowplot::add_sub(plot = figure,
      label = plot.caption,
                     color = "blue",
                     size = 6,
      x = 0.30)
  # ggplot2::grid.a
  #
  # figure <- gridExtra::arrangeGrob(fig_a, fig_b,
  #   fig_c,
  #   ncol = 1, nrow = 3
  # )
  # post-plot ---------------------------------------------------------------

  ggplot2::ggsave(
    plot = figure,
    filename = plot_name,
    units = "mm",
    width = 120,
    height = 150,
    dpi = 300,
    device = "png",
    bg = "white"
  )

  # write csv to disk

  # write maps df
  crunch::write.csv.gz(
    x = na.omit(var_plotting_maps) %>%
      dplyr::mutate_if(is.numeric, round, digits = 3),
    quote = FALSE,
    file = paste0(csv_name, "4maps_.csv.gz"),
    row.names = F
  )

  # write boxplots
  crunch::write.csv.gz(
    x = na.omit(var_plotting_bp) %>%
      dplyr::mutate_if(is.numeric, round, digits = 3),
    quote = FALSE,
    file = paste0(csv_name, "BP_.csv.gz"),
    row.names = F
  )

  # write linear trend
  crunch::write.csv.gz(
    x = na.omit(var_plotting_lt) %>%
      dplyr::mutate_if(is.numeric, round, digits = 3),
    quote = FALSE,
    file = paste0(csv_name, "LT_.csv.gz"),
    row.names = F
  )

  # End ---------------------------------------------------------------------
  # clean
  rm(list = ls())
  gc()
}
