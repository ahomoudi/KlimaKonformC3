#' Calculate dMI (De Martonne Aridity Index) from ReKIS multiple stations
#'
#' This function calculates the dMI (De Martonne Aridity Index)) for climate
#' station data.
#' @param csv.files description
#' @param resolution description
#' @param plot.type description
#' @param axis.scales description
#' @param output_path description
#'
#' @return A PNG
#' @export

clima_dMI_multiple <- function(csv.files,
                               resolution = c(
                                 "Jahr (Jan-Dez)",
                                 "Vegetationsperiode I (April-Juni)",
                                 "Vegetationsperiode II (Juli-Sept)"
                               ),
                               plot.type = c("ts", "bp"),
                               axis.scales,
                               output_path) {
  # read file
  r.file <- lapply(csv.files, data.table::fread)

  # extract temperture and precipition
  r.file <- lapply(r.file, function(x) {
    x[, c(
      "STATID",
      "TT.MM.JJJJ",
      "TM",
      "RK"
    )]
  })

  # data format
  r.file <- lapply(r.file, function(x) {
    x$TT.MM.JJJJ <- as.POSIXct(as.Date(x$TT.MM.JJJJ,
      format = "%d.%m.%Y"
    ))
    return(x)
  })

  r.Results <- lapply(r.file, FUN = function(x) {
    # calculate dMI
    Result <- list()

    for (ivar in resolution) {
      if (ivar == "Jahr (Jan-Dez)") {
        Result[[ivar]] <- x %>%
          dplyr::mutate(Period = lubridate::year(TT.MM.JJJJ)) %>%
          dplyr::group_by(Period) %>%
          dplyr::summarise(
            KTM = mean(TM),
            KRK = sum(RK)
          ) %>%
          dplyr::mutate(dMI = KRK / (KTM + 10))
      }

      if (ivar == "Vegetationsperiode I (April-Juni)") {
        Result[[ivar]] <- x %>%
          dplyr::mutate(Period = lubridate::month(TT.MM.JJJJ)) %>%
          dplyr::filter(Period > 3 & Period < 7) %>%
          dplyr::mutate(Period = paste0(
            lubridate::month(TT.MM.JJJJ),
            "-",
            lubridate::year(TT.MM.JJJJ)
          )) %>%
          dplyr::group_by(Period) %>%
          dplyr::summarise(
            KTM = mean(TM),
            KRK = sum(RK)
          ) %>%
          dplyr::mutate(dMI = KRK / (KTM + 10)) %>%
          dplyr::mutate(Period = as.numeric(stringr::str_sub(Period, -4, -1)))
      }

      if (ivar == "Vegetationsperiode II (Juli-Sept)") {
        Result[[ivar]] <- x %>%
          dplyr::mutate(Period = lubridate::month(TT.MM.JJJJ)) %>%
          dplyr::filter(Period < 4 | Period > 6) %>%
          dplyr::mutate(Period = paste0(
            lubridate::month(TT.MM.JJJJ),
            "-",
            lubridate::year(TT.MM.JJJJ)
          )) %>%
          dplyr::group_by(Period) %>%
          dplyr::summarise(
            KTM = mean(TM),
            KRK = sum(RK)
          ) %>%
          dplyr::mutate(dMI = KRK / (KTM + 10)) %>%
          dplyr::mutate(Period = as.numeric(stringr::str_sub(Period, -4, -1)))
      }

      if (ivar == "Okt-Dez") {
        Result[[ivar]] <- x %>%
          dplyr::mutate(Period = lubridate::month(TT.MM.JJJJ)) %>%
          dplyr::filter(Period < 4 | Period > 6) %>%
          dplyr::mutate(Period = paste0(
            lubridate::month(TT.MM.JJJJ),
            "-",
            lubridate::year(TT.MM.JJJJ)
          )) %>%
          dplyr::group_by(Period) %>%
          dplyr::summarise(
            KTM = mean(TM),
            KRK = sum(RK)
          ) %>%
          dplyr::mutate(dMI = KRK / (KTM + 10)) %>%
          dplyr::mutate(Period = as.numeric(stringr::str_sub(Period, -4, -1)))
      }

      if (ivar == "Jan-Mrz") {
        Result[[ivar]] <- x %>%
          dplyr::mutate(Period = lubridate::month(TT.MM.JJJJ)) %>%
          dplyr::filter(Period > 0 & Period < 4) %>%
          dplyr::mutate(Period = paste0(
            lubridate::month(TT.MM.JJJJ),
            "-",
            lubridate::year(TT.MM.JJJJ)
          )) %>%
          dplyr::group_by(Period) %>%
          dplyr::summarise(
            KTM = mean(TM),
            KRK = sum(RK)
          ) %>%
          dplyr::mutate(dMI = KRK / (KTM + 10)) %>%
          dplyr::mutate(Period = as.numeric(stringr::str_sub(Period, -4, -1)))
      }

      # add other periods here
    }

    return(Result)
  })


  # combine results
  Result <- lapply(r.Results, FUN = function(x) {
    return(dplyr::bind_rows(x, .id = "Scenario"))
  })

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

  plot.title <- unlist(lapply(X = csv.files, FUN = function(x) {
    str_split_custom(x)
  }))

  names(Result) <- plot.title

  plot.title <- paste0(plot.title, collapse = "-")

  Result <- dplyr::bind_rows(Result, .id = "Station")

  if ("ts" %in% plot.type) {
    filename <- paste0(
      output_path,
      plot.title,
      "_multi_LT.png"
    )

    (ggplot2::ggplot(Result) +
      ggplot2::geom_line(
        mapping = ggplot2::aes(
          x = Period,
          y = dMI,
          color = Station,
        ),
        linewidth = 0.3
      ) +
      ggplot2::geom_smooth(
        mapping = ggplot2::aes(
          x = Period,
          y = dMI
        ),
        method = "lm",
        linetype = "longdash",
        linewidth = 0.3,
        se = F
      ) +
      ggplot2::facet_wrap(~Scenario,
        scales = axis.scales
      ) +
      ggplot2::xlab("") +
      ggplot2::ylab("Trockenheitsindex (de Martonne, dMI)") +
      ggplot2::xlim(c(1961, 2020)) +
      ggplot2::ggtitle(plot.title) +
      ggplot2::theme_bw(base_size = 6) +
      ggplot2::theme(plot.title = ggplot2::element_text(
        hjust = 0.5,
        size = 8
      ))) %>%
      ggplot2::ggsave(
        filename = filename,
        units = "mm",
        width = 150,
        height = 100,
        dpi = 300,
        device = "png"
      )
  }

  if ("bp" %in% plot.type) {
    # file name
    filename <- paste0(
      output_path,
      paste0(plot.title, collapse = "-"),
      "_multi_BP.png"
    )
    # add groups column
    Result <- Result %>%
      dplyr::mutate(
        Kyear = dplyr::case_when(
          Period < 1991 & Period > 1960 ~ "1961-1990",
          Period > 1990 & Period < 2021 ~ "1991-2020"
        )
      )



    # add some period index
    (ggplot2::ggplot(Result) +
      ggplot2::geom_boxplot(
        mapping = ggplot2::aes(
          x = Kyear,
          y = dMI,
          color = Station
        ),
        outlier.colour = "red",
        outlier.shape = 4,
        outlier.size = 0.9,
        outlier.alpha = 0.5,
        fatten = 0.5, size = 0.35
      ) +
      ggplot2::stat_boxplot(
        mapping = ggplot2::aes(
          x = Kyear,
          y = dMI,
          color = Station
        ),
        geom = "errorbar",
        linewidth = 0.2
      ) +
      ggplot2::facet_wrap(~Scenario,
        scales = axis.scales
      ) +
      ggplot2::xlab("") +
      ggplot2::ylab("Trockenheitsindex (de Martonne, dMI)") +
      ggplot2::ggtitle(plot.title) +
      ggplot2::theme_bw(base_size = 6) +
      ggplot2::theme(plot.title = ggplot2::element_text(
        hjust = 0.5,
        size = 8
      ))) %>%
      ggplot2::ggsave(
        filename = filename,
        units = "mm",
        width = 150,
        height = 100,
        dpi = 300,
        device = "png"
      )
  }


  # ggplot
}

# csv.file<- csv.files[1]
# output_path
