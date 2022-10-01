#' @title Corine Land Cover
#' @description Corine Land Cover products for KlimaKonfrom Model domain
#' as a dataframe with similar resolution as Monica model.
#' The current CLC 2018 version is v.2020_20u1 was used.
#'
#' @format A data frame with three columns and 19183 rows
#' \describe{
#'   \item{x}{coordinates in x dimensions with 1000 m resoultion}
#'   \item{y}{coordinates in y dimensions with 1000 m resoultion}
#'   \item{Code_18}{values describing the land cover in SpatRaster}
#' }
#' @source Copernicus Land Monitoring Service – part of the Copernicus Programme \url{https://land.copernicus.eu/pan-european/corine-land-cover}
#' 
"land_cover"
