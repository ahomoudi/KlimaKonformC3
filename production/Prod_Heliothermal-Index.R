# plot climate diagramms for "Plauen", "Gera-Leumnitz" und "Zeitz"
library(rdwd)
library(KlimaKonformC3)
library(agroclim)

library(ggplot2)
stations <- c("Plauen", "Gera-Leumnitz", "Zeitz", "Dresden-Klotzsche", "Dresden-Hosterwitz")
stations_lat <- c(50.4819, 50.8812, 51.0314, 51.1280, 51.0221)

istation <- 4
for (istation in 1:length(stations)) {
  no.station <- rdwd::findID(stations[istation], exactmatch = TRUE)

  # select a dataset (e.g. last year's daily climate data from Potsdam city):
  link <- selectDWD(id = no.station, res = "daily", var = "", per = "historical")

  # Actually download that dataset, returning the local storage file name:
  file <- dataDWD(link, read = FALSE)

  # Read the file from the zip folder:
  clim <- readDWD(file, varnames = TRUE)

  clim <- clim[[1]]

  clim <- clim[c(
    "STATIONS_ID", "MESS_DATUM", "RSK.Niederschlagshoehe", "TMK.Lufttemperatur",
    "TXK.Lufttemperatur_Max", "TNK.Lufttemperatur_Min"
  )]

  clim$MESS_DATUM <- as.Date(clim$MESS_DATUM)

  x.df <- dplyr::select(clim, c(
    "MESS_DATUM",
    "TXK.Lufttemperatur_Max",
    "TNK.Lufttemperatur_Min"
  ))

  colnames(x.df) <- c("Datum", "TDMAX", "TDMIN")

  p <- clima_Heliothermal_Index(x.df = x.df, lat = stations_lat[istation])

  ggplot2::ggsave(
    plot = p,
    path = "~/NextCloud/DATA/Shared/KlimaKonform-Results/Heliothermal-Index",
    filename = paste0(stations[istation], ".png"),
    height = 75,
    width = 100,
    units = "mm",
    bg = "white",
    dpi = 600
  )
}
