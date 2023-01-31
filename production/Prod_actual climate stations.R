# plot climate diagramms for "Plauen", "Gera-Leumnitz" und "Zeitz"
library(rdwd)
library(KlimaKonformC3)

stations <- c("Plauen", "Gera-Leumnitz", "Zeitz")


for (istation in stations) {
  rdwd::findID(istation, exactmatch = FALSE)

  # select a dataset (e.g. last year's daily climate data from Potsdam city):
  link <- selectDWD(istation, res = "daily", var = "", per = "historical")

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

  # reduce data size
  clim <- clim[clim$MESS_DATUM > as.Date("1970-12-31") &
    clim$MESS_DATUM < as.Date("2000-02-01"), ]

  data_mon <- clim %>%
    dplyr::mutate(yearmonth = zoo::as.yearmon(MESS_DATUM)) %>%
    dplyr::group_by(yearmonth) %>%
    dplyr::select(-MESS_DATUM) %>%
    dplyr::summarise(
      Precip = sum(RSK.Niederschlagshoehe, na.rm = T),
      Tmin = mean(TNK.Lufttemperatur_Min, na.rm = T),
      Tavg = mean(TMK.Lufttemperatur, na.rm = T),
      Tmax = mean(TXK.Lufttemperatur_Max, na.rm = T)
    )

  # get periods
  r.time <- zoo::as.Date(data_mon$yearmonth)

  time_indices <- periods <- list(
    period1 = which(r.time > as.Date("1970-12-31") &
      r.time < as.Date("2000-02-01")),
    period2 = which(r.time > as.Date("1990-12-31") &
      r.time < as.Date("2020-02-01"))
  )

  # assign periods
  data_mon$Period <- NA
  data_mon$Period[time_indices$period1] <- "1971-2000"
  data_mon$Period[time_indices$period2] <- "1991-2020"


  # calculate periods mean
  data_seasonal <- data_mon %>%
    dplyr::mutate(Date = zoo::as.Date(yearmonth)) %>%
    dplyr::mutate(MONTH = lubridate::month(Date)) %>%
    dplyr::group_by(Period, MONTH) %>%
    dplyr::select(-yearmonth, -Date) %>%
    dplyr::summarise(
      Precip = mean(Precip, na.rm = T),
      Tmin = mean(Tmin, na.rm = T),
      Tavg = mean(Tavg, na.rm = T),
      Tmax = mean(Tmax, na.rm = T)
    )

  # clean the data
  data_seasonal <- na.omit(data_seasonal)


  # get long term means
  data_longterm <- clim %>%
    dplyr::mutate(year = lubridate::year(MESS_DATUM)) %>%
    dplyr::group_by(year) %>%
    dplyr::select(-MESS_DATUM) %>%
    dplyr::summarise(
      Precip = sum(RSK.Niederschlagshoehe, na.rm = T),
      Tmin = mean(TNK.Lufttemperatur_Min, na.rm = T),
      Tavg = mean(TMK.Lufttemperatur, na.rm = T),
      Tmax = mean(TXK.Lufttemperatur_Max, na.rm = T)
    )


  # get periods
  r.time <- data_longterm$year

  time_indices <- periods <- list(
    period1 = which(r.time > 1970 &
      r.time < 2001), # Referenz
    period2 = which(r.time > 1990 &
      r.time < 2021),
    period3 = which(r.time > 2020 &
      r.time < 2051),
    period4 = which(r.time > 2069 &
      r.time < 2100)
  )

  # assign periods
  data_longterm$Period <- NA
  data_longterm$Period[time_indices$period1] <- "1971-2000"
  data_longterm$Period[time_indices$period2] <- "1991-2020"

  data_longterm <- data_longterm %>%
    dplyr::group_by(Period) %>%
    dplyr::select(-year) %>%
    dplyr::summarise(
      Precip = mean(Precip, na.rm = T),
      Tmin = mean(Tmin, na.rm = T),
      Tavg = mean(Tavg, na.rm = T),
      Tmax = mean(Tmax, na.rm = T)
    )



  # clean the data
  data_longterm <- na.omit(data_longterm)

  # get P1
  p1 <- clima_diagramm_abs(
    data = data_seasonal %>% dplyr::filter(Period == "1971-2000"),
    temp_precip_mean = data_longterm[1, c(4, 2)],
    location = istation,
    language = "DE",
    run_id = "Messeung"
  )



  # get P2
  data <- ((data_seasonal %>% dplyr::filter(Period == "1991-2020"))[, -1] -
    (data_seasonal %>% dplyr::filter(Period == "1971-2000"))[, -1]) %>%
    dplyr::mutate(Period = "1991-2020") %>%
    dplyr::mutate(MONTH = 1:12)

  p2 <- clima_diagramm_change(
    data = data,
    temp_precip_mean = (data_longterm[2, c(4, 2)] - data_longterm[1, c(4, 2)]),
    location = istation,
    language = "DE",
    run_id = "Messeung"
  )

  ggplot2::ggsave(
    plot = cowplot::plot_grid(p1, p2,
      ncol = 2,
      nrow = 1
    ),
    path = "~/NextCloud/DATA/Shared/KlimaKonform-Results/Klimastationen",
    filename = paste0(istation, ".png"),
    height = 75,
    width = 150,
    units = "mm",
    bg = "white",
    dpi = 600
  )
}
