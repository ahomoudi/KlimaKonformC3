#' @title Plotting Heliothermal Index
#'
#' @param x.df A data frame containing Datum, tmax (daily max temp),
#' tmean (daily mean temp)
#' @param lat A single value of latitude
#' @author Ahmed Homoudi
#' @return  ggplot2 plot
#' @export
clima_Heliothermal_Index <- function(x.df, lat) {

  # test

  hi_res <- x.df %>%
    dplyr::mutate(
      YEAR = lubridate::year(as.Date(Datum)),
      LAT = lat
    ) %>%
    dplyr::group_by(YEAR) %>%
    dplyr::mutate(HI = Heliothermal_Index(tmax, tmean, dates = Datum)) %>%
    dplyr::distinct(YEAR, HI)
  hi_res<-na.omit(hi_res)

  hi_res<-hi_res[hi_res$HI!=0,]

  p <- ggplot2::ggplot(hi_res) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = YEAR, y = HI), size = 1) +
    ggformula::geom_spline(
      data = na.omit(hi_res),
      mapping = ggplot2::aes(x = YEAR, y = HI),
      inherit.aes = TRUE
    ) +
    ggplot2::theme_bw(base_size = 8) +
    ggplot2::scale_x_continuous(
      breaks = seq(1880, 2022, 15),
      expand = c(0, 0),
      limits = c(1880, 2022)
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 2500, 250),
      expand = c(0, 0),
      limits = c(0, 2500)
    ) +
    ggplot2::xlab("") +
    ggplot2::ylab("Huglin Heliothermal Index (HI)")


  return(list(p, hi_res))
}
