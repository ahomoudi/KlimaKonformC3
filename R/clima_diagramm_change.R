#' @title Climate Diagram (Relative)
#' @description
#' Plot the relative climate diagram for grid points from the climate data used to run the Monica models.
#' The function receives a data frame, preferable prepared by the R code
#' '/production/Prod_klima_diagramm_4corners.R'. The input data are representing grids
#' in the KlimaKonform model region or at least Regions. All meta data required to plot this are either
#' the production script or the function here.
#'
#' @param data A data frame containing the data to be plotted. The data should contain
#' min, max, and mean temperature and precipitation.
#' @param temp_precip_mean A vector of the long term average of temperature and precipitation.
#' @param location A character referring to the location of the data, e.g., "Greiz".
#' @param run_id A character variable either pointing out the run id, please refer
#'  to the simulation setup file.
#' @param language A character variable either "EN" or "DE", to specify the languages of plots.
#'
#' @author Ahmed Homoudi
#' @return  ggplot2 plot
#' @import stats
#' @importFrom utils globalVariables write.table
#' @export
#' @seealso  clima_diagramm_change
#'
clima_diagramm_change <- function(data,
                                  temp_precip_mean,
                                  location,
                                  language,
                                  run_id) {



  # Niederschlagssumme (mm)  Rainfall Sum (mm)
  # Lufttemperatur max, mit, min Air temperature max, mean, min


  # plot
  y.axis.max <- max(data[,c("Tmin","Tavg","Tmax")], na.rm = T)
  y.axis.min <- min(data[,c("Tmin","Tavg","Tmax")], na.rm = T)

  ylim.prim <- setting_nice_limits(y.axis.min, y.axis.max)

  #getting which is greater
  if(abs(ylim.prim[1])>abs(ylim.prim[2])){
    ylim.prim[2]<-abs(ylim.prim[1])
  }else{
    ylim.prim[1]<- -1 * ylim.prim[2]
  }

  ylim.sec<-set_sec_axis(ylim.prim,y.sec.range = range(data$Precip))

  # ylim.prim <- c(-3, 3) # in this example, temperature
  # ylim.sec <- c(-15, 15) # in this example, precipitation

  b <- diff(ylim.prim) / diff(ylim.sec)
  a <- ylim.prim[1] - b * ylim.sec[1] # there was a bug here

  # language
  if (language == "DE") {
    message("Producing Plots in German")

    # change to month
    data$MONTH <- month.abb.DE[data$MONTH]
    data$MONTH <- factor(data$MONTH, levels = month.abb.DE)

    # Melt
    data <- data %>%
      tidyr::pivot_longer(-c("Period", "MONTH"),
        names_to = "variables",
        values_to = "Kvalue"
      )

    data$variables <- paste(data$variables, data$Period)

    # subtitles
    Tavg <- unname(unlist(round(temp_precip_mean[1], digits = 1)))
    Pavg <- unname(unlist(round(temp_precip_mean[2], digits = 0)))

    Tavg <- ifelse(Tavg > 0, paste0("+", Tavg), paste0("-", Tavg))
    Pavg <- ifelse(Pavg > 0, paste0("+", Pavg), paste0("-", Pavg))

    # plotting
    fig <- ggplot2::ggplot() +
      ggplot2::geom_col(
        data = data %>%
          dplyr::filter(stringr::str_detect(pattern = "Precip", variables)),
        mapping = ggplot2::aes(
          x = as.numeric(MONTH),
          y = Kvalue * b,
          fill = variables
        )
      ) +
      ggplot2::scale_fill_manual(
        values = "blue",
        labels = "\u00c4nderung N", ,
        guide = ggplot2::guide_legend(
          title = NULL,
          keywidth = grid::unit(5, "mm"),
          keyheight = grid::unit(5, "mm")
        )
      ) +
      ggplot2::geom_path(
        data = data %>%
          dplyr::filter(!stringr::str_detect(pattern = "Precip", variables)) %>%
          dplyr::mutate(variables = factor(variables, levels = rev(unique(data$variables)))),
        mapping = ggplot2::aes(
          x = as.numeric(MONTH),
          y = Kvalue,
          group = variables,
          linetype = variables
        ),
        color = "red"
      ) +
      ggplot2::scale_linetype_manual(
        values = c("dashed", "solid", "dotted"),
        labels = c(
          "\u00c4nderung Tmax",
          "\u00c4nderung Tmit",
          "\u00c4nderung Tmin"
        ),
        guide = ggplot2::guide_legend(
          title = NULL,
          ncol = 2,
          keywidth = grid::unit(5, "mm"),
          keyheight = grid::unit(5, "mm")
        )
      ) +
      ggplot2::scale_y_continuous(
        limits = ylim.prim,
        name = expression("Lufttemperatur max, mit, min "(degree * C)),
        breaks = seq(ylim.prim[1], ylim.prim[2], 1),
        sec.axis = ggplot2::sec_axis(~ . / b,
          breaks = seq(ylim.prim[1], ylim.prim[2], 1) / b,
          name = "Niederschlagssumme (mm)"
        )
      ) +
      ggplot2::scale_x_continuous(
        labels = month.abb.DE,
        breaks = 1:12
      ) +
      ggplot2::theme_linedraw(base_size = 8) +
      ggplot2::xlab("") +
      ggplot2::theme(axis.title.y.right = ggplot2::element_text(angle = 90)) +
      ggplot2::theme(
        legend.position = "bottom",
        legend.margin = ggplot2::margin(0, 0, 0, 0, "mm"),
        legend.box.margin = ggplot2::margin(0, 0, 0, 0, "mm"),
        legend.text = ggplot2::element_text(
          color = "black",
          size = 5
        ),
        plot.margin = ggplot2::margin(2, 2, 2, 2, "mm"),
        axis.title = ggplot2::element_text(
          color = "black",
          size = 6
        ),
        axis.text = ggplot2::element_text(
          color = "black",
          size = 5
        )
      )
    # title
    figt <- grid::textGrob(
      label = paste0(
        "\u00c4nderung mittl. Temperatur, Niederschlag \n ",
        unique(data$Period),
        " vs. 1971-2000"
      ),
      gp = grid::gpar(
        fontsize = 6,
        col = "black"
      )
    )
  } else if (language == "EN") {
    message("Producing Plots in English")

    # change to month
    data$MONTH <- month.abb[data$MONTH]
    data$MONTH <- factor(data$MONTH, levels = month.abb)

    # Melt
    data <- data %>%
      tidyr::pivot_longer(-c("Period", "MONTH"),
        names_to = "variables",
        values_to = "Kvalue"
      )

    data$variables <- paste(data$variables, data$Period)

    # subtitles
    Tavg <- unname(unlist(round(temp_precip_mean[1], digits = 1)))
    Pavg <- unname(unlist(round(temp_precip_mean[2], digits = 0)))


    # plotting
    fig <- ggplot2::ggplot() +
      ggplot2::geom_col(
        data = data %>%
          dplyr::filter(stringr::str_detect(pattern = "Precip", variables)),
        mapping = ggplot2::aes(
          x = as.numeric(MONTH),
          y = Kvalue * b,
          fill = variables
        )
      ) +
      ggplot2::scale_fill_manual(
        values = "blue",
        labels = labels_replace,
        guide = ggplot2::guide_legend(title = NULL)
      ) +
      ggplot2::geom_path(
        data = data %>%
          dplyr::filter(!stringr::str_detect(pattern = "Precip", variables)) %>%
          dplyr::mutate(variables = factor(variables, levels = rev(unique(data$variables)))),
        mapping = ggplot2::aes(
          x = as.numeric(MONTH),
          y = Kvalue,
          group = variables,
          linetype = variables
        ),
        color = "red"
      ) +
      ggplot2::scale_linetype_manual(
        values = c("dashed", "solid", "dotted"),
        guide = ggplot2::guide_legend(
          title = NULL,
          ncol = 2
        )
      ) +
      ggplot2::scale_y_continuous(
        limits = ylim.prim,
        name = expression("Air temperature max, mean, min "(degree * C)),
        breaks = seq(-5, 50, 5),
        sec.axis = ggplot2::sec_axis(~ . / b,
          breaks = seq(-5, 50, 5) / b,
          name = "Rainfall (mm)"
        )
      ) +
      ggplot2::scale_x_continuous(
        labels = month.abb.DE,
        breaks = 1:12
      ) +
      ggplot2::theme_linedraw(base_size = 8) +
      ggplot2::xlab("") +
      ggplot2::theme(axis.title.y.right = ggplot2::element_text(angle = 90)) +
      ggplot2::theme(
        legend.position = "bottom",
        legend.margin = ggplot2::margin(0, 0, 0, 0, "mm"),
        legend.box = "horizontal",
        legend.box.just = "top",
        legend.box.margin = ggplot2::margin(0, 0, 0, 0, "mm"),
        legend.text = ggplot2::element_text(
          color = "black",
          size = 5
        ),
        plot.margin = ggplot2::margin(2, 2, 2, 2, "mm"),
        axis.title = ggplot2::element_text(
          color = "black",
          size = 6
        ),
        axis.text = ggplot2::element_text(
          color = "black",
          size = 5
        )
      )
  } else {
    stop("Please select a language, either \"EN\" or \"DE\"")
  }


  # temp
  figa <- grid::textGrob(
    label = as.expression(eval(bquote(.(Tavg) ~ degree * C),
      envir = list(x = 0)
    )),
    just = "left",
    gp = grid::gpar(
      fontsize = 6,
      col = "red"
    )
  )

  # location
  figb <- grid::textGrob(
    label = location,
    just = "centre",
    gp = grid::gpar(
      fontsize = 6,
      fontface = "bold",
      col = "black"
    )
  )

  # precip
  figc <- grid::textGrob(
    label = as.expression(eval(bquote(.(Pavg) ~ "mm"),
      envir = list(x = 0)
    )),
    just = "right",
    gp = grid::gpar(
      fontsize = 6,
      col = "blue"
    )
  )

  # combine all
  figm <- cowplot::plot_grid(figa, figb, figc,
    ncol = 3,
    rel_widths = c(0.2, 0.6, 0.2),
    nrow = 1
  )


  p2 <- cowplot::plot_grid(figt,
    figm,
    fig,
    ncol = 1,
    nrow = 3,
    rel_heights = c(0.1, 0.05, 1.1)
  )


  # p2

  return(p2)
}
