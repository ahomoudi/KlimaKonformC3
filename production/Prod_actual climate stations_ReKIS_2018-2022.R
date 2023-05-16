# plot climate diagramms for "Plauen", "Gera-Leumnitz" und "Zeitz"
# library(rdwd)
library(KlimaKonformC3)
library(tidyverse)

stations <- c(
  "Plauen",
  "Gera-Leumnitz",
  "Zeitz",
  # "Greiz-Dölau",
  "Dresden-Klotzsche",
  "Dresden-Hosterwitz"
)
# 3946
# 1612
# 5750
# 1762,1762,Greiz-Dölau,50.6249,12.1777,270,3
# 1763,1763,Greiz-Dölau (Stausee),50.626,12.176,273,4
# 1048
# 1050
stationsID <- c(
  03946,
  01612,
  5750,
  # 01762,
  01048,
  01050
)

stations_lat <- c(50.4819, 50.8812, 51.0314, 51.1280, 51.0221)

istation <- stationsID[1]

zip_file <- list.files(
  path = "/media/HDD/Daten/WHK2/Data/DWDData",
  pattern = ".zip$",
  full.names = TRUE
)


# Monthly -----------------------------------------------------------------

CSV_result1 <- list()
CSV_result2 <- list()
for (istation in stationsID) {
  # preparing data ----------------------------------------------------------


  sub_files <- grep(istation,
    x = zip_file,
    value = TRUE
  )

  df1 <- readr::read_delim(
    unzip(
      sub_files[1],
      grep("produkt", unzip(sub_files[1], list = TRUE)[, 1], value = TRUE)
    ),
    delim = ";",
    escape_double = FALSE,
    col_types = cols(MESS_DATUM = col_date(format = "%Y%m%d")),
    trim_ws = TRUE
  )


  df2 <- readr::read_delim(
    unzip(
      sub_files[2],
      grep("produkt", unzip(sub_files[2], list = TRUE)[, 1], value = TRUE)
    ),
    delim = ";",
    escape_double = FALSE,
    col_types = cols(MESS_DATUM = col_date(format = "%Y%m%d")),
    trim_ws = TRUE
  )

  df_all <- rbind(df1, df2)

  df_all[df_all == -999] <- NA

  names(df_all) <- stringr::str_trim(names(df_all), side = c("both", "left", "right"))

  rm(df1, df2)
  # average of 1961 - 1990

  sub_df <- df_all %>%
    dplyr::select(STATIONS_ID, MESS_DATUM, TXK, TMK, TNK, SDK, RSK) %>%
    dplyr::mutate(unique_month = paste0(
      lubridate::year(MESS_DATUM),
      "_",
      lubridate::month(MESS_DATUM)
    ))


  longterm <- sub_df %>%
    dplyr::group_by(unique_month) %>%
    dplyr::mutate(MONTH = lubridate::month(MESS_DATUM)) %>%
    dplyr::mutate(YEAR = lubridate::year(MESS_DATUM)) %>%
    # dplyr::select(-MESS_DATUM) %>%
    dplyr::summarise(
      SDK = sum(SDK, na.rm = T),
      Precip = sum(RSK, na.rm = T),
      Tmin = mean(TNK, na.rm = T),
      Tavg = mean(TMK, na.rm = T),
      Tmax = mean(TXK, na.rm = T),
      MONTH = unique(MONTH),
      YEAR = unique(YEAR)
    )

  longtermmean <- longterm %>%
    dplyr::filter(YEAR > 1960 & YEAR < 1991) %>%
    dplyr::group_by(MONTH) %>%
    dplyr::summarise(
      Sonnenscheindauer = mean(SDK, na.rm = T),
      Niederschlag = mean(Precip, na.rm = T),
      TemperaturMin = mean(Tmin, na.rm = T),
      TemperaturAvg = mean(Tavg, na.rm = T),
      TemperaturMax = mean(Tmax, na.rm = T)
    )

  df20182022 <- longterm %>%
    dplyr::filter(YEAR > 2017 & YEAR < 2023) %>%
    dplyr::mutate(DATUM = as.Date(
      paste0(YEAR, sprintf("%02d", MONTH), "15"),
      "%Y%m%d"
    ))

  names(df20182022)[2:6] <- c(
    "Sonnenscheindauer [Std.]",
    "Niederschlag [mm]",
    "TemperaturMin [°C]",
    "TemperaturAvg [°C]",
    "TemperaturMax [°C]"
  )

  df20182022 <- df20182022 %>%
    dplyr::select(
      "DATUM",
      "Sonnenscheindauer [Std.]",
      "Niederschlag [mm]",
      "TemperaturMin [°C]",
      "TemperaturAvg [°C]",
      "TemperaturMax [°C]"
    ) %>%
    tidyr::pivot_longer(-DATUM,
      names_to = "Knames",
      values_to = "Kvalues"
    )

  # monthly_ts -------------------------------------------------------------------

  plotting_monthly_ts <- df20182022 %>%
    dplyr::mutate(
      ymin = case_when(
        Knames == "Niederschlag [mm]" ~ 0,
        Knames == "Sonnenscheindauer [Std.]" ~ 0,
        Knames == "TemperaturMax [°C]" ~ -20,
        Knames == "TemperaturAvg [°C]" ~ -20,
        Knames == "TemperaturMin [°C]" ~ -20
      ),
      ymax = case_when(
        Knames == "Niederschlag [mm]" ~ 75,
        Knames == "Sonnenscheindauer [Std.]" ~ 200,
        Knames == "TemperaturMax [°C]" ~ 40,
        Knames == "TemperaturAvg [°C]" ~ 40,
        Knames == "TemperaturMin [°C]" ~ 40
      )
    )


  (ggplot(plotting_monthly_ts, aes(x = DATUM, y = Kvalues)) +
    facet_wrap(~Knames,
      scales = "free"
    ) +
    geom_col(data = subset(plotting_monthly_ts, Knames == "Niederschlag [mm]"), fill = "blue") +
    geom_col(data = subset(plotting_monthly_ts, Knames == "Sonnenscheindauer [Std.]"), fill = "#999900") +
    geom_line(data = subset(plotting_monthly_ts, Knames == "TemperaturMax [°C]"), color = "red") +
    geom_line(data = subset(plotting_monthly_ts, Knames == "TemperaturAvg [°C]"), color = "green") +
    geom_line(data = subset(plotting_monthly_ts, Knames == "TemperaturMin [°C]"), color = "black") +
    xlab("") +
    ylab("") +
    ggtitle(stations[which(istation == stationsID)]) +
    scale_x_date( # breaks =  date_breaks("6 months"),
      date_minor_breaks = "1 months",
      date_labels = "%b %Y",
      date_breaks = "6 months"
    ) +
    theme_bw(base_size = 8) +
    geom_blank(aes(y = ymin)) +
    geom_blank(aes(y = ymax)) +
    theme(
      axis.text.x = element_text(
        size = 4,
        colour = "black",
        angle = 45,
        hjust = 1,
        # vjust = 0,
        margin = margin(0.1, 0, 0, 0, "cm")
      ),
      axis.text.y = element_text(
        size = 4,
        colour = "black"
      ),
      plot.title = element_text(hjust = 0.5, size = 8),
      strip.text = element_text(size = 6)
    )) %>%
    ggplot2::ggsave(
      path = "~/NextCloud/DATA/Shared/KlimaKonform-Results/20182022/monthly_ts/",
      filename = paste0(stations[which(istation == stationsID)], ".png"),
      height = 100,
      width = 150,
      units = "mm",
      bg = "white",
      dpi = 600
    )

  # monthly_anomaly --------------------------------------------------------------

  names(longtermmean)[2:6] <- c(
    "Sonnenscheindauer [Std.]",
    "Niederschlag [mm]",
    "TemperaturMin [°C]",
    "TemperaturAvg [°C]",
    "TemperaturMax [°C]"
  )

  plotting_monthly_anomly <- df20182022 %>%
    dplyr::mutate(MONTH = lubridate::month(DATUM)) %>%
    dplyr::left_join(
      longtermmean %>%
        tidyr::pivot_longer(-MONTH,
          names_to = "Knames",
          values_to = "Kvalues_cycle"
        ),
      by = c("Knames", "MONTH")
    )

  plotting_monthly_anomly$Kanomaly <- plotting_monthly_anomly$Kvalues -
    plotting_monthly_anomly$Kvalues_cycle


  plotting_monthly_anomly$Kanomaly[which(plotting_monthly_anomly$Knames == "Sonnenscheindauer [Std.]" &
    plotting_monthly_anomly$Kvalues < 1e-10)] <- NA


  plotting_monthly_anomly <- plotting_monthly_anomly %>%
    dplyr::mutate(
      ymin = case_when(
        Knames == "Niederschlag [mm]" ~ 75,
        Knames == "Sonnenscheindauer [Std.]" ~ -100,
        Knames == "TemperaturMax [°C]" ~ -15,
        Knames == "TemperaturAvg [°C]" ~ -15,
        Knames == "TemperaturMin [°C]" ~ -15
      ),
      ymax = case_when(
        Knames == "Niederschlag [mm]" ~ -75,
        Knames == "Sonnenscheindauer [Std.]" ~ 150,
        Knames == "TemperaturMax [°C]" ~ 15,
        Knames == "TemperaturAvg [°C]" ~ 15,
        Knames == "TemperaturMin [°C]" ~ 15
      )
    )
  if (istation != 5750) {
    (ggplot(plotting_monthly_anomly, aes(x = DATUM, y = Kanomaly)) +
      facet_wrap(~Knames,
        scales = "free"
      ) +
      geom_col(data = subset(plotting_monthly_anomly, Knames == "Niederschlag [mm]"), fill = "blue") +
      geom_col(data = subset(plotting_monthly_anomly, Knames == "Sonnenscheindauer [Std.]"), fill = "#999900") +
      geom_line(data = subset(plotting_monthly_anomly, Knames == "TemperaturMax [°C]"), color = "red") +
      geom_line(data = subset(plotting_monthly_anomly, Knames == "TemperaturAvg [°C]"), color = "green") +
      geom_line(data = subset(plotting_monthly_anomly, Knames == "TemperaturMin [°C]"), color = "black") +
      xlab("") +
      ylab("") +
      ggtitle(stations[which(istation == stationsID)]) +
      scale_x_date( # breaks =  date_breaks("6 months"),
        date_minor_breaks = "1 months",
        date_labels = "%b %Y",
        date_breaks = "6 months"
      ) +
      theme_bw(base_size = 8) +
      geom_blank(aes(y = ymin)) +
      geom_blank(aes(y = ymax)) +
      theme(
        axis.text.x = element_text(
          size = 4,
          colour = "black",
          angle = 45,
          hjust = 1,
          # vjust = 0,
          margin = margin(0.1, 0, 0, 0, "cm")
        ),
        axis.text.y = element_text(
          size = 4,
          colour = "black"
        ),
        plot.title = element_text(hjust = 0.5, size = 8),
        strip.text = element_text(size = 6)
      ) +
      geom_hline(aes(yintercept = 0), linewidth = 0.25)
    ) %>%
      ggplot2::ggsave(
        path = "~/NextCloud/DATA/Shared/KlimaKonform-Results/20182022/monthly_anomaly/",
        filename = paste0(stations[which(istation == stationsID)], ".png"),
        height = 100,
        width = 150,
        units = "mm",
        bg = "white",
        dpi = 600
      )
  } else {
    (ggplot(plotting_monthly_anomly, aes(x = DATUM, y = Kanomaly)) +
      facet_wrap(~Knames,
        scales = "free"
      ) +
      geom_col(data = subset(plotting_monthly_anomly, Knames == "Niederschlag [mm]"), fill = "blue") +
      # geom_col(data = subset(plotting_monthly_anomly, Knames == "Sonnenscheindauer [Std.]"), fill = "#999900") +
      geom_line(data = subset(plotting_monthly_anomly, Knames == "TemperaturMax [°C]"), color = "red") +
      geom_line(data = subset(plotting_monthly_anomly, Knames == "TemperaturAvg [°C]"), color = "green") +
      geom_line(data = subset(plotting_monthly_anomly, Knames == "TemperaturMin [°C]"), color = "black") +
      xlab("") +
      ylab("") +
      ggtitle(stations[which(istation == stationsID)]) +
      scale_x_date( # breaks =  date_breaks("6 months"),
        date_minor_breaks = "1 months",
        date_labels = "%b %Y",
        date_breaks = "6 months"
      ) +
      theme_bw(base_size = 8) +
      geom_blank(aes(y = ymin)) +
      geom_blank(aes(y = ymax)) +
      theme(
        axis.text.x = element_text(
          size = 4,
          colour = "black",
          angle = 45,
          hjust = 1,
          # vjust = 0,
          margin = margin(0.1, 0, 0, 0, "cm")
        ),
        axis.text.y = element_text(
          size = 4,
          colour = "black"
        ),
        plot.title = element_text(hjust = 0.5, size = 8),
        strip.text = element_text(size = 6)
      ) +
      geom_hline(aes(yintercept = 0), linewidth = 0.25)
    ) %>%
      ggplot2::ggsave(
        path = "~/NextCloud/DATA/Shared/KlimaKonform-Results/20182022/monthly_anomaly/",
        filename = paste0(stations[which(istation == stationsID)], ".png"),
        height = 100,
        width = 150,
        units = "mm",
        bg = "white",
        dpi = 600
      )
  }

  CSV_result1[[which(istation == stationsID)]] <- plotting_monthly_anomly %>%
    dplyr::select(DATUM, Knames, Kvalues) %>%
    # dplyr::mutate(ID=1:n())%>%
    tidyr::pivot_wider(
      id_cols = DATUM,
      names_from = Knames,
      values_from = Kvalues
    ) %>% # remove ID columns
    dplyr::mutate_if(is.numeric, round, digits = 3)


  CSV_result2[[which(istation == stationsID)]] <- plotting_monthly_anomly %>%
    dplyr::select(DATUM, Knames, Kanomaly) %>%
    # dplyr::mutate(ID=1:n())%>%
    tidyr::pivot_wider(
      id_cols = DATUM,
      names_from = Knames,
      values_from = Kanomaly
    ) %>% # remove ID columns
    dplyr::mutate_if(is.numeric, round, digits = 3)
}

# HI-Index ----------------------------------------------------------------
for (istation in stationsID) {
  # preparing data ----------------------------------------------------------


  sub_files <- grep(istation,
    x = zip_file,
    value = TRUE
  )

  df1 <- readr::read_delim(
    unzip(
      sub_files[1],
      grep("produkt", unzip(sub_files[1], list = TRUE)[, 1], value = TRUE)
    ),
    delim = ";",
    escape_double = FALSE,
    col_types = cols(MESS_DATUM = col_date(format = "%Y%m%d")),
    trim_ws = TRUE
  )


  df2 <- readr::read_delim(
    unzip(
      sub_files[2],
      grep("produkt", unzip(sub_files[2], list = TRUE)[, 1], value = TRUE)
    ),
    delim = ";",
    escape_double = FALSE,
    col_types = cols(MESS_DATUM = col_date(format = "%Y%m%d")),
    trim_ws = TRUE
  )

  df_all <- rbind(df1, df2)

  df_all[df_all == -999] <- NA

  names(df_all) <- stringr::str_trim(names(df_all), side = c("both", "left", "right"))

  rm(df1, df2)





  x.df <- dplyr::select(df_all, c(
    "MESS_DATUM",
    "TXK",
    "TMK"
  )) %>%
    dplyr::filter(MESS_DATUM < as.Date("2023-01-01")) %>%
    dplyr::filter(MESS_DATUM > as.Date("2017-12-31"))

  colnames(x.df) <- c("Datum", "tmax", "tmean")

  Results <- clima_Heliothermal_Index(x.df = x.df, lat = stations_lat[which(istation == stationsID)])

  ggplot2::ggsave(
    plot = Results[[1]],
    path = "~/NextCloud/DATA/Shared/KlimaKonform-Results/20182022/HI-INDEX/",
    filename = paste0(stations[which(istation == stationsID)], ".png"),
    height = 75,
    width = 100,
    units = "mm",
    bg = "white",
    dpi = 600
  )

  print(stations[which(istation == stationsID)])
  print(unlist(Results[[2]][, 2]))
}


# April-September  --------------------------------------------------------

for (istation in stationsID) {
  # preparing data ----------------------------------------------------------


  sub_files <- grep(istation,
    x = zip_file,
    value = TRUE
  )

  df1 <- readr::read_delim(
    unzip(
      sub_files[1],
      grep("produkt", unzip(sub_files[1], list = TRUE)[, 1], value = TRUE)
    ),
    delim = ";",
    escape_double = FALSE,
    col_types = cols(MESS_DATUM = col_date(format = "%Y%m%d")),
    trim_ws = TRUE
  )


  df2 <- readr::read_delim(
    unzip(
      sub_files[2],
      grep("produkt", unzip(sub_files[2], list = TRUE)[, 1], value = TRUE)
    ),
    delim = ";",
    escape_double = FALSE,
    col_types = cols(MESS_DATUM = col_date(format = "%Y%m%d")),
    trim_ws = TRUE
  )

  df_all <- rbind(df1, df2)

  df_all[df_all == -999] <- NA

  names(df_all) <- stringr::str_trim(names(df_all), side = c("both", "left", "right"))

  rm(df1, df2)
  # average of 1961 - 1990

  sub_df <- df_all %>%
    dplyr::select(STATIONS_ID, MESS_DATUM, TXK, TMK, TNK, SDK, RSK) %>%
    dplyr::mutate(unique_month = paste0(
      lubridate::year(MESS_DATUM),
      "_",
      lubridate::month(MESS_DATUM)
    ))


  longterm <- sub_df %>%
    dplyr::group_by(unique_month) %>%
    dplyr::mutate(MONTH = lubridate::month(MESS_DATUM)) %>%
    dplyr::mutate(YEAR = lubridate::year(MESS_DATUM)) %>%
    # dplyr::select(-MESS_DATUM) %>%
    dplyr::summarise(
      SDK = sum(SDK, na.rm = T),
      Precip = sum(RSK, na.rm = T),
      Tmin = mean(TNK, na.rm = T),
      Tavg = mean(TMK, na.rm = T),
      Tmax = mean(TXK, na.rm = T),
      MONTH = unique(MONTH),
      YEAR = unique(YEAR)
    )

  longtermmean <- longterm %>%
    dplyr::filter(YEAR > 1960 & YEAR < 1991) %>%
    dplyr::group_by(MONTH) %>%
    dplyr::summarise(
      Sonnenscheindauer = mean(SDK, na.rm = T),
      Niederschlag = mean(Precip, na.rm = T),
      TemperaturMin = mean(Tmin, na.rm = T),
      TemperaturAvg = mean(Tavg, na.rm = T),
      TemperaturMax = mean(Tmax, na.rm = T)
    )

  df20182022 <- longterm %>%
    dplyr::filter(YEAR > 2017 & YEAR < 2023) %>%
    dplyr::mutate(DATUM = as.Date(
      paste0(YEAR, sprintf("%02d", MONTH), "15"),
      "%Y%m%d"
    ))

  names(df20182022)[2:6] <- c(
    "Sonnenscheindauer [Std.]",
    "Niederschlag [mm]",
    "TemperaturMin [°C]",
    "TemperaturAvg [°C]",
    "TemperaturMax [°C]"
  )

  df20182022 <- df20182022 %>%
    dplyr::select(
      "DATUM",
      "Sonnenscheindauer [Std.]",
      "Niederschlag [mm]",
      "TemperaturMin [°C]",
      "TemperaturAvg [°C]",
      "TemperaturMax [°C]"
    ) %>%
    tidyr::pivot_longer(-DATUM,
      names_to = "Knames",
      values_to = "Kvalues"
    )

  # ts -------------------------------------------------------------------

  plotting_monthly_ts <- df20182022 %>%
    dplyr::mutate(
      ymin = case_when(
        Knames == "Niederschlag [mm]" ~ 0,
        Knames == "Sonnenscheindauer [Std.]" ~ 0,
        Knames == "TemperaturMax [°C]" ~ -20,
        Knames == "TemperaturAvg [°C]" ~ -20,
        Knames == "TemperaturMin [°C]" ~ -20
      ),
      ymax = case_when(
        Knames == "Niederschlag [mm]" ~ 75,
        Knames == "Sonnenscheindauer [Std.]" ~ 200,
        Knames == "TemperaturMax [°C]" ~ 40,
        Knames == "TemperaturAvg [°C]" ~ 40,
        Knames == "TemperaturMin [°C]" ~ 40
      )
    )

  plotting_monthly_ts <- plotting_monthly_ts %>%
    dplyr::mutate(stuidped_month = lubridate::month(DATUM))


  plotting_monthly_ts$Kvalues[which(between(plotting_monthly_ts$stuidped_month, 4, 9))] <- NA

  (ggplot(plotting_monthly_ts, aes(x = DATUM, y = Kvalues)) +
    facet_wrap(~Knames,
      scales = "free"
    ) +
    geom_col(data = subset(plotting_monthly_ts, Knames == "Niederschlag [mm]"), fill = "blue") +
    geom_col(data = subset(plotting_monthly_ts, Knames == "Sonnenscheindauer [Std.]"), fill = "#999900") +
    geom_line(data = subset(plotting_monthly_ts, Knames == "TemperaturMax [°C]"), color = "red") +
    geom_line(data = subset(plotting_monthly_ts, Knames == "TemperaturAvg [°C]"), color = "green") +
    geom_line(data = subset(plotting_monthly_ts, Knames == "TemperaturMin [°C]"), color = "black") +
    xlab("") +
    ylab("") +
    ggtitle(stations[which(istation == stationsID)]) +
    scale_x_date( # breaks =  date_breaks("6 months"),
      date_minor_breaks = "1 months",
      date_labels = "%b %Y",
      date_breaks = "6 months"
    ) +
    theme_bw(base_size = 8) +
    geom_blank(aes(y = ymin)) +
    geom_blank(aes(y = ymax)) +
    theme(
      axis.text.x = element_text(
        size = 4,
        colour = "black",
        angle = 45,
        hjust = 1,
        # vjust = 0,
        margin = margin(0.1, 0, 0, 0, "cm")
      ),
      axis.text.y = element_text(
        size = 4,
        colour = "black"
      ),
      plot.title = element_text(hjust = 0.5, size = 8),
      strip.text = element_text(size = 6)
    )) %>%
    ggplot2::ggsave(
      path = "~/NextCloud/DATA/Shared/KlimaKonform-Results/20182022/Apr-Sep_ts/",
      filename = paste0(stations[which(istation == stationsID)], ".png"),
      height = 100,
      width = 150,
      units = "mm",
      bg = "white",
      dpi = 600
    )

  # anomaly --------------------------------------------------------------

  names(longtermmean)[2:6] <- c(
    "Sonnenscheindauer [Std.]",
    "Niederschlag [mm]",
    "TemperaturMin [°C]",
    "TemperaturAvg [°C]",
    "TemperaturMax [°C]"
  )

  plotting_monthly_anomly <- df20182022 %>%
    dplyr::mutate(MONTH = lubridate::month(DATUM)) %>%
    dplyr::left_join(
      longtermmean %>%
        tidyr::pivot_longer(-MONTH,
          names_to = "Knames",
          values_to = "Kvalues_cycle"
        ),
      by = c("Knames", "MONTH")
    )

  plotting_monthly_anomly$Kanomaly <- plotting_monthly_anomly$Kvalues -
    plotting_monthly_anomly$Kvalues_cycle


  plotting_monthly_anomly$Kanomaly[which(plotting_monthly_anomly$Knames == "Sonnenscheindauer [Std.]" &
    plotting_monthly_anomly$Kvalues < 1e-10)] <- NA


  plotting_monthly_anomly <- plotting_monthly_anomly %>%
    dplyr::mutate(
      ymin = case_when(
        Knames == "Niederschlag [mm]" ~ 75,
        Knames == "Sonnenscheindauer [Std.]" ~ -100,
        Knames == "TemperaturMax [°C]" ~ -15,
        Knames == "TemperaturAvg [°C]" ~ -15,
        Knames == "TemperaturMin [°C]" ~ -15
      ),
      ymax = case_when(
        Knames == "Niederschlag [mm]" ~ -75,
        Knames == "Sonnenscheindauer [Std.]" ~ 150,
        Knames == "TemperaturMax [°C]" ~ 15,
        Knames == "TemperaturAvg [°C]" ~ 15,
        Knames == "TemperaturMin [°C]" ~ 15
      )
    )


  plotting_monthly_anomly <- plotting_monthly_anomly %>%
    dplyr::mutate(stuidped_month = lubridate::month(DATUM))


  plotting_monthly_anomly$Kvalues[which(between(plotting_monthly_anomly$stuidped_month, 4, 9))] <- NA
  if (istation != 5750) {
    (ggplot(plotting_monthly_anomly, aes(x = DATUM, y = Kanomaly)) +
      facet_wrap(~Knames,
        scales = "free"
      ) +
      geom_col(data = subset(plotting_monthly_anomly, Knames == "Niederschlag [mm]"), fill = "blue") +
      geom_col(data = subset(plotting_monthly_anomly, Knames == "Sonnenscheindauer [Std.]"), fill = "#999900") +
      geom_line(data = subset(plotting_monthly_anomly, Knames == "TemperaturMax [°C]"), color = "red") +
      geom_line(data = subset(plotting_monthly_anomly, Knames == "TemperaturAvg [°C]"), color = "green") +
      geom_line(data = subset(plotting_monthly_anomly, Knames == "TemperaturMin [°C]"), color = "black") +
      xlab("") +
      ylab("") +
      ggtitle(stations[which(istation == stationsID)]) +
      scale_x_date( # breaks =  date_breaks("6 months"),
        date_minor_breaks = "1 months",
        date_labels = "%b %Y",
        date_breaks = "6 months"
      ) +
      theme_bw(base_size = 8) +
      geom_blank(aes(y = ymin)) +
      geom_blank(aes(y = ymax)) +
      theme(
        axis.text.x = element_text(
          size = 4,
          colour = "black",
          angle = 45,
          hjust = 1,
          # vjust = 0,
          margin = margin(0.1, 0, 0, 0, "cm")
        ),
        axis.text.y = element_text(
          size = 4,
          colour = "black"
        ),
        plot.title = element_text(hjust = 0.5, size = 8),
        strip.text = element_text(size = 6)
      ) +
      geom_hline(aes(yintercept = 0), linewidth = 0.25)
    ) %>%
      ggplot2::ggsave(
        path = "~/NextCloud/DATA/Shared/KlimaKonform-Results/20182022/Apr-Sep_anomly/",
        filename = paste0(stations[which(istation == stationsID)], ".png"),
        height = 100,
        width = 150,
        units = "mm",
        bg = "white",
        dpi = 600
      )
  } else {
    (ggplot(plotting_monthly_anomly, aes(x = DATUM, y = Kanomaly)) +
      facet_wrap(~Knames,
        scales = "free"
      ) +
      geom_col(data = subset(plotting_monthly_anomly, Knames == "Niederschlag [mm]"), fill = "blue") +
      # geom_col(data = subset(plotting_monthly_anomly, Knames == "Sonnenscheindauer [Std.]"), fill = "#999900") +
      geom_line(data = subset(plotting_monthly_anomly, Knames == "TemperaturMax [°C]"), color = "red") +
      geom_line(data = subset(plotting_monthly_anomly, Knames == "TemperaturAvg [°C]"), color = "green") +
      geom_line(data = subset(plotting_monthly_anomly, Knames == "TemperaturMin [°C]"), color = "black") +
      xlab("") +
      ylab("") +
      ggtitle(stations[which(istation == stationsID)]) +
      scale_x_date( # breaks =  date_breaks("6 months"),
        date_minor_breaks = "1 months",
        date_labels = "%b %Y",
        date_breaks = "6 months"
      ) +
      theme_bw(base_size = 8) +
      geom_blank(aes(y = ymin)) +
      geom_blank(aes(y = ymax)) +
      theme(
        axis.text.x = element_text(
          size = 4,
          colour = "black",
          angle = 45,
          hjust = 1,
          # vjust = 0,
          margin = margin(0.1, 0, 0, 0, "cm")
        ),
        axis.text.y = element_text(
          size = 4,
          colour = "black"
        ),
        plot.title = element_text(hjust = 0.5, size = 8),
        strip.text = element_text(size = 6)
      ) +
      geom_hline(aes(yintercept = 0), linewidth = 0.25)
    ) %>%
      ggplot2::ggsave(
        path = "~/NextCloud/DATA/Shared/KlimaKonform-Results/20182022/monthly_anomaly/",
        filename = paste0(stations[which(istation == stationsID)], ".png"),
        height = 100,
        width = 150,
        units = "mm",
        bg = "white",
        dpi = 600
      )
  }
}



# CSV ---------------------------------------------------------------------

names(CSV_result1)

names(CSV_result1) <- stations

CSV_result1 <- bind_rows(CSV_result1, .id = "STATION")

crunch::write.csv.gz(
  x = CSV_result1 %>% # remove ID columns
    dplyr::mutate(DATUM = as.Date(DATUM, "%d-%m-%Y")) %>%
    group_by(STATION) %>%
    dplyr::arrange(DATUM, .by_group = TRUE) %>%
    dplyr::mutate_if(is.numeric, round, digits = 3),
  quote = FALSE,
  file = "~/NextCloud/DATA/Shared/KlimaKonform-Results/20182022/monthly_ts/KlimaDaten_2018-2022_Absoulate.csv",
  row.names = F
)


names(CSV_result2)

names(CSV_result2) <- stations

CSV_result2 <- bind_rows(CSV_result2, .id = "STATION")

crunch::write.csv.gz(
  x = CSV_result2 %>% # remove ID columns
    dplyr::mutate(DATUM = as.Date(DATUM, "%d-%m-%Y")) %>%
    group_by(STATION) %>%
    dplyr::arrange(DATUM, .by_group = TRUE) %>%
    dplyr::mutate_if(is.numeric, round, digits = 3),
  quote = FALSE,
  file = "~/NextCloud/DATA/Shared/KlimaKonform-Results/20182022/monthly_anomaly/KlimaDaten_2018-2022_anomaly.csv",
  row.names = F
)



print("END")

plot(df_all$MESS_DATUM, df_all$SDK, xaxt = "n", type = "l")
axis(1, df_all$MESS_DATUM, format(df_all$MESS_DATUM, "%Y %m"), cex.axis = .7)
plot(df_all$MESS_DATUM, df_all$TMK, xaxt = "n", type = "l")
axis(1, df_all$MESS_DATUM, format(df_all$MESS_DATUM, "%Y %m"), cex.axis = .7)
