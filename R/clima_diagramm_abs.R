#' @title Plot the absolute climate diagram for grid points from the climate data used to run the Monica models.
#' @description  The function receives a data frame, preferable prepared by the
#' R code '/production/Prod_klima_diagramm_4corners.R'. The input data are representing grids
#' the KlimaKonform model region. All meta data required to plot this are either
#' the production script or the function here.
#' @param data A data frame containing the daily data to be plotted. The data should contain
#' min, max, and mean temperature and precipitation. For only three locations, e.g., MO, MM, and MU.
#' @param run_id A character variable either pointing out the run id, please refer
#'  to the simulation setup file.
#' @param output_path A string pointing to the output directory for output plot
#' @author Ahmed Homoudi
#' @return  PNG
#' @import stats
#' @importFrom utils globalVariables write.table
#' @export
#' @seealso  clima_diagramm_change
clima_diagramm_abs <- function(data,
                                    run_id,
                                    output_path) {
  zoo::as.Date()
}

