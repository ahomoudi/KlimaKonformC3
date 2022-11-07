#' @title Plot climate data used to run the Monica models.
#' @description  The function receives a data frame, preferable prepaped by the
#' R code '/production/Prod_klima_corners.R'. The input data are rpresenting  grids
#' the KlimaKonform model region. All meta data required to plot this are either
#' the production script or the function here.
#' @param data A data frame containg the data to be plotted.
#' @param run_id A character variable either pointing out the run id, please refer
#'  to the simulation setup file.
#' @param output_path A string pointing to the output directory for output plot
#' @author Ahmed Homoudi
#' @return  PNG
#' @import stats
#' @importFrom utils globalVariables write.table
#' @export

clima_plot_cornors<-function(data,
                             run_id,
                             output_path){


  climate_vars<-colnames(data)[-c(1:3)]


# limits and colours  -----------------------------------------------------

  y.axis.limits<-data.frame(climate_vars=climate_vars,
                            lower.lim  = c(rep(-25,3), 0 , 0, 0 , 0,   300),
                            higher.lim = c(rep( 45,3), 15, 0, 10, 100, 1200))

  # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9",
                  "#009E73", "#F0E442", "#0072B2",
                  "#D55E00", "#CC79A7")
# plotting  ---------------------------------------------------------------
  data$Date<-as.Date(data$Date)

  for (ivar in 1:length(climate_vars)){

    y.axis.max <- max(data[,climate_vars[ivar]], na.rm = T)
    y.axis.min <- min(data[,climate_vars[ivar]], na.rm = T)

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


    (ggplot2::ggplot()+
         ggplot2::geom_line(data = data,
                            mapping = ggplot2::aes(x=Date,
                                                   y= eval(parse(text = climate_vars[ivar])),
                                                   col = corners,
                                                   group=corners),
                            size = 0.2)+
        ggplot2::theme_bw(base_size = 6) +

       ggplot2::scale_y_continuous(
         limits = y.axis.limits,
         expand = c(0, 0)
       ) +
       ggplot2::scale_x_date(
         date_breaks = "10 years",
         minor_breaks = "5 years",
         expand = c(0, 0),
         date_labels = "%Y"
       )+
         ggplot2::scale_colour_manual(values=cbbPalette)+
       ggplot2::xlab("")+
       ggplot2::ylab("")+
       ggplot2::ggtitle(label = climate_vars[ivar])+

        ggplot2::labs(color='Gitter')
       )%>%
      ggplot2::ggsave(filename = paste0(output_path,
                                        run_id,"_",
                                        climate_vars[ivar],".png"),
                      units = "mm",
                      width = 150,
                      height = 80,
                      dpi = 300,
                      device = "png")

  }


  message(paste0(run_id, "plotting is done :)"))

  # End ---------------------------------------------------------------------
  # clean
  rm(list = ls())
  gc()

}
