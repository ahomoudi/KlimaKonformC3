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
#' @param temp_precip_mean A vector of the long term average of temperature and pürecipition
#' @param run_id A character variable either pointing out the run id, please refer
#'  to the simulation setup file.
#' @param output_path A string pointing to the output directory for output plot.
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
                               run_id) {



  # change to month

  data$MONTH<-month.abb[data$MONTH]
  data$MONTH<-factor(data$MONTH, levels = month.abb)




  # plot

  ylim.prim <- c(-5,50)   # in this example, temperature
  ylim.sec <- c(-10,100)    # in this example, precipitation

  b <- diff(ylim.prim)/diff(ylim.sec)
  a <- ylim.prim[1] - b*ylim.sec[1] # there was a bug here

ggplot(climate, aes(Month, Precip)) +
  geom_col() +
  geom_line(aes(y = a + Temp*b), color = "red") +
  scale_y_continuous("Precipitation", sec.axis = sec_axis(~ (. - a)/b, name = "Temperature")) +
  scale_x_continuous("Month", breaks = 1:12) +
  ggtitle("Climatogram for Oslo (1961-1990)")


  unique(data$Period)

  ggplot2::ggplot(data = data)+
    ggplot2::geom_col(mapping = ggplot2::aes(x = MONTH,
                                             y = Precip))+
    ggplot2::geom_path(mapping = ggplot2::aes(x = MONTH,
                                              y = Tmin,
                                              group=1),
                       linetype = "dotted")+
    ggplot2::geom_path(mapping = ggplot2::aes(x = MONTH,
                                              y = Tavg,
                                              group=1),
                       linetype = "solid")+
    ggplot2::geom_path(mapping = ggplot2::aes(x = MONTH,
                                              y = Tmax,
                                              group=1),
                       linetype = "dashed")+

    ggplot2::scale_y_continuous(limits = ylim.prim,
                                sec.axis = ggplot2::sec_axis(~ (. - a)/b, name = "precipitation") )


}
