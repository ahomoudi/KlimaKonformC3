#' @title DEM or DGM
#' @description  Das Digitale Geländemodell Gitterweite 200 m (DGM200) beschreibt
#' die Geländeformen der Erdoberfläche durch eine in einem regelmäßigen
#' Gitter angeordnete, in Lage und Höhe georeferenzierte Punktmenge.
#' Die Gitterweite beträgt 200 m. Der Datenbestand deckt das Territorium
#' der Bundesrepublik Deutschland ab.
#' Die Nutzung der Geodaten und Geodatendienste wird durch die "Verordnung zur Festlegung
#' der Nutzungsbestimmungen für die Bereitstellung von Geodaten des Bundes (GeoNutzV)"
#' \url{http://www.geodatenzentrum.de/auftrag/pdf/geonutz.pdf} geregelt.
#'
#' @format A data frame with three columns and 19172 rows
#' \describe{
#'   \item{x}{coordinates in x dimensions with 1000 m resoultion}
#'   \item{y}{coordinates in y dimensions with 1000 m resoultion}
#'   \item{DEM}{values describing the elevation value (DEM)}
#' }
#' @source Bundesamt für Kartographie und Geodäsie (BKG) \url{http://www.bkg.bund.de}
#'
"DEM_data"
