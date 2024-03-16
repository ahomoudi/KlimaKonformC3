# plot climate diagramms for "Plauen", "Gera-Leumnitz" und "Zeitz"
library(KlimaKonformC3)

input_dir <- "/media/HDD/Daten/WHK2/Data/ReKIS"

csv.files <- list.files(
  path = input_dir,
  pattern = ".csv$",
  recursive = T,
  full.names = T
)
output_path <- "~/NextCloud/DATA/Shared/KlimaKonform-Results/Klimastationen/4Report"
str_split_custom <- function(X) {
  first <- unlist(stringr::str_split(X,
    pattern = "/"
  ))

  Xr <- unlist(stringr::str_split(first[length(first)],
    pattern = "_"
  ))

  Xr[length(Xr)] <- unlist(stringr::str_split(Xr[length(Xr)],
    pattern = "[.]"
  ))[1]

  return(Xr)
}

# stations <- c("Plauen", "Gera-Leumnitz", "Zeitz", "Dresden-Klotzsche", "Dresden-Hosterwitz")

# istation <- stations[3]
ifile <- csv.files[1]
for (ifile in csv.files) {
  # read file
  r.file <- data.table::fread(ifile)


  # data format
  r.file$TT.MM.JJJJ <- as.POSIXct(as.Date(r.file$TT.MM.JJJJ,
    format = "%d.%m.%Y"
  ))


  data_mon <- r.file %>%
    dplyr::mutate(yearmonth = zoo::as.yearmon(TT.MM.JJJJ)) %>%
    dplyr::group_by(yearmonth) %>%
    dplyr::select(-TT.MM.JJJJ) %>%
    dplyr::summarise(
      Precip = sum(RK, na.rm = T),
      Tmin = mean(TN, na.rm = T),
      Tavg = mean(TM, na.rm = T),
      Tmax = mean(TX, na.rm = T)
    )

  # get periods
  r.time <- zoo::as.Date(data_mon$yearmonth)

  time_indices <- periods <- list(
    period1 = which(r.time > as.Date("1970-12-31") &
      r.time < as.Date("1990-02-01")),
    period2 = which(r.time > as.Date("1990-12-31") &
      r.time < as.Date("2020-02-01"))
  )

  # assign periods
  data_mon$Period <- NA
  data_mon$Period[time_indices$period1] <- "1971-1990"
  data_mon$Period[time_indices$period2] <- "1991-2020"

  # clean the data
  # data_seasonal <- na.omit(data_seasonal)
  data_mon <- na.omit(data_mon)

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


   data_longterm <- r.file %>%
     dplyr::mutate(YEAR =lubridate::year(TT.MM.JJJJ)) %>%
     dplyr::group_by(YEAR) %>%
     dplyr::select(-TT.MM.JJJJ) %>%
     dplyr::summarise(
       Precip = sum(RK, na.rm = T),
       Tmin = mean(TN, na.rm = T),
       Tavg = mean(TM, na.rm = T),
       Tmax = mean(TX, na.rm = T))%>%
     dplyr::mutate(Period = dplyr::case_when(YEAR > 1970 & YEAR < 2001 ~ "1971-1990",
                                             YEAR > 1990 & YEAR < 2021 ~ "1991-2020"))%>%
                                             dplyr::group_by(Period)%>%
                                               dplyr::summarise(
                                                 Precip = mean(Precip , na.rm = T),
                                                 Tmin = mean(Tmin, na.rm = T),
                                                 Tavg = mean(Tavg, na.rm = T),
                                                 Tmax = mean(Tmax, na.rm = T))
  data_longterm <- na.omit(data_longterm)

  data.table::fwrite(data_longterm, file = paste0(output_path,
                                                  str_split_custom(ifile), ".txt"))

  # get P1
  data <- data_seasonal %>% dplyr::filter(Period == "1971-1990")
  p1 <- clima_diagramm_abs(
    data = data,
    temp_precip_mean = data_longterm[1, c(4, 2)],
    location = str_split_custom(ifile),
    language = "DE",
    run_id = "ReKIS"
  )



  # get P2
  data <- ((data_seasonal %>% dplyr::filter(Period == "1991-2020"))[, -1] -
    (data_seasonal %>% dplyr::filter(Period == "1971-1990"))[, -1]) %>%
    dplyr::mutate(Period = "1991-2020") %>%
    dplyr::mutate(MONTH = 1:12)

  p2 <- clima_diagramm_change(
    data = data,
    temp_precip_mean = (data_longterm[2, c(4, 2)] - data_longterm[1, c(4, 2)]),
    location = str_split_custom(ifile),
    language = "DE",
    run_id = "ReKIS"
  )

  ggplot2::ggsave(
    plot = cowplot::plot_grid(p1, p2,
      ncol = 2,
      nrow = 1
    ),
    path = output_path,
    filename = paste0(str_split_custom(ifile), ".png"),
    height = 75,
    width = 150,
    units = "mm",
    bg = "white",
    dpi = 600
  )

  readr::write_csv(data_seasonal, paste0(output_path, "/",
                                               str_split_custom(ifile),
                                               ".csv"))
}
