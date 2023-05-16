# plot climate diagramms for "Plauen", "Gera-Leumnitz" und "Zeitz"
library(rdwd)
library(KlimaKonformC3)

library(ggplot2)
stations <- c("Plauen", "Gera-Leumnitz", "Zeitz", "Dresden-Klotzsche", "Dresden-Hosterwitz")
stations_lat <- c(50.4819, 50.8812, 51.0314, 51.1280, 51.0221)


# A -----------------------------------------------------------------------
Results_A <- list()
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
    "TMK.Lufttemperatur"
  ))

  colnames(x.df) <- c("Datum", "tmax", "tmean")

  Results_A[[istation]] <- clima_Heliothermal_Index(x.df = x.df, lat = stations_lat[istation])

  ggplot2::ggsave(
    plot = Results_A[[istation]][[1]],
    path = "~/NextCloud/DATA/Shared/KlimaKonform-Results/Heliothermal-Index/A",
    filename = paste0(stations[istation], ".png"),
    height = 75,
    width = 100,
    units = "mm",
    bg = "white",
    dpi = 600
  )
}

# B -----------------------------------------------------------------------
Results_B <- list()
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
    "TMK.Lufttemperatur"
  ))

  colnames(x.df) <- c("Datum", "tmax", "tmean")
  x.df[, c(1, 2)] <- x.df[, c(1, 2)] + 1.5

  Results_B[[istation]] <- clima_Heliothermal_Index(x.df = x.df, lat = stations_lat[istation])

  ggplot2::ggsave(
    plot = Results_B[[istation]][[1]],
    path = "~/NextCloud/DATA/Shared/KlimaKonform-Results/Heliothermal-Index/B",
    filename = paste0(stations[istation], ".png"),
    height = 75,
    width = 100,
    units = "mm",
    bg = "white",
    dpi = 600
  )
}

# C -----------------------------------------------------------------------
Results_C <- list()
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
    "TMK.Lufttemperatur"
  ))

  colnames(x.df) <- c("Datum", "tmax", "tmean")
  x.df[, c(1, 2)] <- x.df[, c(1, 2)] + 2.0
  Results_C[[istation]] <- clima_Heliothermal_Index(x.df = x.df, lat = stations_lat[istation])

  ggplot2::ggsave(
    plot = Results_C[[istation]][[1]],
    path = "~/NextCloud/DATA/Shared/KlimaKonform-Results/Heliothermal-Index/C",
    filename = paste0(stations[istation], ".png"),
    height = 75,
    width = 100,
    units = "mm",
    bg = "white",
    dpi = 600
  )
}

unlink("DWDdata/", recursive = TRUE)
# others ------------------------------------------------------------------

data <- readRDS("inst/HI_data.RDS")
stations <- c("Plauen", "Gera-Leumnitz", "Zeitz", "Dresden-Klotzsche", "Dresden-Hosterwitz")
library(ggplot2)
library(patchwork)
library(ggpmisc)
for (i in 1:length(stations)) {
  station_data <- list(
    data[[1]][[i]][[2]],
    data[[2]][[i]][[2]],
    data[[3]][[i]][[2]]
  )
  names(station_data) <- c("A", "B", "C")

  station_data <- data.table::rbindlist(station_data, idcol = "ID")
  station_data$GROUP <- NA

  station_data$GROUP[station_data$YEAR >= 1960 & station_data$YEAR <= 1990] <- "1960-1990"
  station_data$GROUP[station_data$YEAR >= 1991 & station_data$YEAR <= 2020] <- "1991-2020"
  station_data$HI2 <- station_data$HI
  station_data$HI2[station_data$HI < 500] <- NA

  p1 <- ggplot2::ggplot(station_data) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        x = YEAR,
        y = HI2,
        color = ID
      ),
      size = 0.3
    ) +
    ggplot2::geom_smooth(
      mapping = ggplot2::aes(
        x = YEAR,
        y = HI2,
        color = ID
      ),
      method = "lm",
      # linetype = "longdash",
      size = 0.3,
      se = F
    ) +
    # stat_poly_line(aes(x = YEAR,
    #                    y = HI2,
    #                    color = ID)) +
    stat_poly_eq(
      size = 1.25,
      aes(
        x = YEAR,
        y = HI2,
        color = ID,
        label = paste(after_stat(eq.label),
          after_stat(rr.label),
          sep = "*\", \"*"
        )
      )
    ) +
    ggplot2::theme_bw(base_size = 5) +
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
    ggplot2::ylab("Huglin Heliothermal Index (HI)") +
    theme(legend.title = element_blank()) +
    ggtitle("(a)") # ; p1

  p2 <- ggplot(data = subset(station_data, !is.na(GROUP))) +
    geom_boxplot(
      mapping = aes(x = GROUP, fill = ID, y = HI2),
      size = 0.3,
      outlier.size = 0.3,
      outlier.colour = "red"
    ) +
    theme_bw(base_size = 5) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 2500, 250),
      expand = c(0, 0),
      limits = c(0, 2500)
    ) +
    xlab("") +
    ggplot2::ylab("Huglin Heliothermal Index (HI)") +
    theme(legend.title = element_blank()) +
    ggtitle("(b)") # ; p2

  ggplot2::ggsave(
    plot = p1 + p2,
    path = "~/NextCloud/DATA/Shared/KlimaKonform-Results/Heliothermal-Index/Auswertung",
    filename = paste0(stations[i], ".png"),
    height = 80,
    width = 120,
    units = "mm",
    bg = "white",
    dpi = 600
  )
}
