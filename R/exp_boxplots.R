#' @title Plotting boxplots from experiment files
#' @description A function that receives three netCDF files, and variable name,
#' statistical ensemble member and plot boxplots of 4 different period and place
#' it  in the desired the out put directory in which the output is placed.
#' @param netCDF.files The netCDF files that contain similar simulation
#' variables. They should be provided including the full path to the files
#' @param variable inside these netCDF files, e.g., AET
#' @param region A string referring to the region to plot; "total",
#' "Vogtlandkreis", "Burgenlandkreis", "Greiz", and "Altenburger Land".
#' @param landcover A numeric variable indicating which land cover should be
#' considered. To consider all land cover give 1000.
#' @param y.axis.limits A vector of length 2L to produce all plots with similar
#' y axis limits.
#' @param language A character variable either "EN" or "DE", to specify the
#' languages of plots
#' @param run_id A character variable either "2ter", "3ter", "4ter", or "6ter"
#' #' specifying the which run is used to produce the ensemble.
#' @param output_path A string pointing to the output directory,
#'  it should end with "/"
#' @param output_csv A string pointing to the output directory for the csv
#' file that is produced by pre-processing netCDF files
#' @author Ahmed Homoudi
#' @return  PNG
#' @import stats
#' @importFrom utils globalVariables write.table
#' @export
exp_boxplots <- function(netCDF.files,
                         variable,
                         region,
                         landcover,
                         y.axis.limits = NA,
                         language,
                         run_id,
                         output_path,
                         output_csv = NULL) {
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
  r.rast <- lapply(X = netCDF.files, FUN = terra::rast)

  # get names
  r.names <- unlist(lapply(netCDF.files, FUN = function(x) {
    substr(
      x,
      nchar(x) - 17,
      nchar(x) - 3
    )
  }))

  # CO2-Düngeeffekt, keine Bewässerung
  if (language == "DE") {
    r.names <- gsub(
      "CO2",
      " CO2-D\u00FCngeeffekt / ",
      r.names
    )

    r.names <- gsub(
      "irrigation",
      " Bew\u00E4sserung",
      r.names
    )

    r.names <- gsub(
      "0",
      "(-)",
      r.names
    )

    r.names <- gsub(
      "1",
      "(+)",
      r.names
    )
  } else if (language == "EN") {
    r.names <- gsub(
      "CO2",
      " CO2-Fertilization / ",
      r.names
    )

    r.names <- gsub(
      "irrigation",
      " Irrigation",
      r.names
    )

    r.names <- gsub(
      "0",
      "(-)",
      r.names
    )

    r.names <- gsub(
      "1",
      "(+)",
      r.names
    )
  }

  names(r.rast) <- r.names

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

  # prepare boxplots data-----------------------------------------------------

  var_plotting <- lapply(r.rast, boxplot_df) %>%
    purrr::imap(.x = ., ~ purrr::set_names(.x, c("ID", "Period", .y))) %>%
    purrr::reduce(dplyr::left_join, by = c("ID", "Period")) %>%
    tidyr::pivot_longer(
      cols = -c("ID", "Period"),
      names_to = "Experiment",
      values_to = "Kvalue"
    )

  # clean some memory
  gc(verbose = F)

  # meta data ---------------------------------------------------------------

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
    plot.caption <- paste(
      paste0(
        "\u00a9 KlimaKonform ", lubridate::year(Sys.time()),
        ". ",
        LC_name
      ),
      paste0("Quelle: Experiment No. ", run_id),
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
    plot.caption <- paste(
      paste0(
        "\u00a9 KlimaKonform ", lubridate::year(Sys.time()),
        ". ",
        LC_name
      ),
      paste0("Source: Experiment No.", run_id),
      sep = "\n"
    )
  }


  # change name of  -------------------------------------------------

  if (language == "DE") {
    legend.title <- "Experimente"
  }

  if (language == "EN") {
    legend.title <- "Experiments"
  }

  #
  # plots and csv name  -----------------------------------------------------

  uniquefilename<-unlist(stringr::str_extract_all(netCDF.files[1],pattern = paste0("(?<=",
                                                          variable,
                                                          "_).+(?=_197)")))

  # define plot name
  if (exists("output_path")) {
    plot_name <- paste0(
      output_path,
      variable, "_",
      uniquefilename, "_",
      run_id, "_lauf_",
      "4XEXP_BP_.png"
    )
  } else {
    plot_name <- paste0(
      variable, "_",
      uniquefilename, "_",
      run_id, "_lauf_",
      "4XEXP_BP_.png"
    )
  }
  # define csv name
  if (!is.null(output_csv)) {
    csv_name <- paste0(
      output_csv,
      variable, "_",
      "experiments_",
      run_id, "_lauf_",
      "4XEXP_BP_.csv.gz"
    )
  } else {
    csv_name <- paste0(
      variable, "_",
      "experiments_",
      run_id, "_lauf_",
      "4XEXP_BP_.csv.gz"
    )
  }

  # pre-plot --------------------------------------------------------------------
  # read logo
  # KlimaKonform_img <- project_logo()
  # grid::rasterGrob(KlimaKonform_img)

  if (is.na(y.axis.limits)) {
    csv_file <- system.file(paste0(run_id, "_lauf.csv"),
                            package = "KlimaKonformC3"
    )

    csv_file <- readr::read_csv(csv_file, show_col_types = F)

    csv_file <- csv_file[grep(x = csv_file$variable, pattern = variable), ]

    y.axis.max <- max(csv_file$maximum, na.rm = T)
    y.axis.min <- min(csv_file$minimum, na.rm = T)

    y.limits <- setting_nice_limits(y.axis.min, y.axis.max)

    y.axis.max <- y.limits[2]
    y.axis.min <- y.limits[1]

    y.axis.limits <- c(y.axis.min, y.axis.max)
  }

  # colors
  rcps_colours <- c("#1b9e77", "#d95f02", "#7570b3", "#984ea3")

  # plot --------------------------------------------------------------------

  figure <- ggplot2::ggplot(
    data = var_plotting,
    mapping = ggplot2::aes(
      x = Period,
      y = Kvalue,
      color = Experiment
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
      linewidth = 0.3
    ) +
    ggplot2::scale_color_manual(
      values = rcps_colours,
      labels = stringr::str_wrap(r.names,
        width = 24
      )
    ) +
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
      legend.key.size = ggplot2::unit(7, "mm"),
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
    width = 140,
    height = 90,
    dpi = 300,
    device = "png"
  )

  if (!is.null(output_csv)) {
    # write csv to disk
    crunch::write.csv.gz(
      x = var_plotting[, -1] %>% # remove ID columns
        dplyr::mutate_if(is.numeric, round, digits = 3),
      quote = FALSE,
      file = csv_name,
      row.names = F
    )
  }
  # End ---------------------------------------------------------------------
  # clean
  rm(list = ls())
  gc()
}
