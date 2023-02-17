#' @title Plotting Heliothermal Index
#'
#' @param x.df A data frame containing Datum, TDMAX (daily max temp),
#' TDMIN  (daily min temp)
#' @param lat A single value of latitude
#' @author Ahmed Homoudi
#' @return  ggplot2 plot
#' @import stats
#' @importFrom utils globalVariables write.table
#' @importFrom agroclim hi
#' @importFrom ggformula geom_spline
#' @export
clima_Heliothermal_Index <- function(x.df, lat) {
  hi_res <- x.df %>%
    dplyr::mutate(
      YEAR = lubridate::year(as.Date(Datum)),
      LAT = lat
    ) %>%
    dplyr::group_by(YEAR) %>%
    dplyr::mutate(HI = agroclim::hi(
      mx = TDMAX,
      mn = TDMIN,
      dates = Datum,
      lati = unique(LAT)
    )) %>%
    dplyr::distinct(YEAR, HI)

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
      breaks = seq(0, 2000, 250),
      expand = c(0, 0),
      limits = c(0, 2000)
    ) +
    ggplot2::xlab("") +
    ggplot2::ylab("Huglin Heliothermal Index (HI)")


  return(p)
}
