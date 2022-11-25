#' @title Climate Diagram (Absolute)
#' @description
#' Plot the absolute climate diagram for grid points from the climate data used to run the Monica models.
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
clima_diagramm_abs <- function(data,
                               temp_precip_mean,
                               location,
                               language,
                               run_id) {



  # Niederschlagssumme (mm)  Rainfall Sum (mm)
  # Lufttemperatur max, mit, min Air temperature max, mean, min


  # plot
  ylim.prim <- c(-5, 50) # in this example, temperature
  ylim.sec <- c(-10, 100) # in this example, precipitation

  b <- diff(ylim.prim) / diff(ylim.sec)
  a <- ylim.prim[1] - b * ylim.sec[1] # there was a bug here

  # language
  if (language == "DE") {
    message("Producing Plots in German")

    # change to month
    data$MONTH <- month.abb.DE[data$MONTH]
    data$MONTH <- factor(data$MONTH, levels = month.abb.DE)

    labels_replace <- function(x) {
      stringr::str_replace(
        pattern = "Precip",
        replacement = "N",
        x
      )
    }
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
      ggplot2::coord_fixed(ylim = c(-5, 70), clip = "off") +
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
        guide = ggplot2::guide_legend(title = NULL)
      ) +
      ggplot2::scale_y_continuous(
        limits = ylim.prim,
        name = expression("Lufttemperatur max, mit, min "(degree * C)),
        breaks = seq(-5, 50, 5),
        sec.axis = ggplot2::sec_axis(~ . / b,
          breaks = seq(-5, 50, 5) / b,
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
        plot.margin = ggplot2::margin(5, 2, 2, 2, "pt"),
        plot.title = ggplot2::element_text(
          color = "black",
          hjust = 0.5,
          vjust = 1
        )
      ) +


      # title and subtitles
      ggplot2::labs(title = unique(data$Period)) +
      ggplot2::annotate("text",
        x = 0.75,
        y = 54,
        color = "red",
        label = as.expression(eval(bquote(.(Tavg) ~ degree * C), envir = list(x = 0)))
      ) +
      ggplot2::annotate("text",
        x = 12,
        y = 50,
        color = "blue",
        label = as.expression(eval(bquote(.(Pavg) ~ "mm"), envir = list(x = 0)))
      ) +
      ggplot2::annotate("text",
        x = 6,
        y = 50,
        color = "black",
        label = location
      )



    fig
  } else if (language == "EN") {
    message("Producing Plots in English")
  } else {
    stop("Please select a language, either \"EN\" or \"DE\"")
  }

  return(fig)
}
