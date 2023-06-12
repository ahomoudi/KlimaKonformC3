# mean of SpatRaster
f1_mean <- function(x) {
  y <- terra::global(x, "mean", na.rm = T)
  return(y)
}



# max of spatRaster in vector forum
f1_max <- function(x) {
  y <- terra::global(x, "max", na.rm = T)

  return(y)
}

# min
f1_min <- function(x) {
  y <- terra::global(x, "min", na.rm = T)

  return(y)
}

# replace

f1_replace_values_in_SpatRaster_layers <- function(x, IDs) {
  layers <- terra::nlyr(x)

  for (ilyr in 1:layers) {
    x[[ilyr]][IDs] <- NA
  }

  return(x)
}
#' A function to receive a number and round up or down according to the next number indicated by roundTo
#' @param x A number to round
#' @param roundTo   number indicating to which number to round, e.g., 5 , or 10
#' @param dir A binary indicating rounding  down zero or up one
#' @return rounded number
#' @export
#' @author theforestecologist in StackOverflow
#' @examples
#' x <- c(25, 50, 75, 125, 150, 175, 220)
#' roundTo <- 5
#' dir <- 1
#' round_custom(
#'   x,
#'   roundTo,
#'   dir
#' )
#' @source \url{https://stackoverflow.com/a/32508105/13818750}
round_custom <- function(x, roundTo, dir = 1) {
  if (dir == 1) { ## ROUND UP
    x + (roundTo - x %% roundTo)
  } else {
    if (dir == 0) { ## ROUND DOWN
      x - (x %% roundTo)
    }
  }
}

# transformation function
scaleFUN <- function(x) sprintf("%.0f", x)


#
setting_nice_limits <- function(y.axis.min, y.axis.max) {
  if (c(y.axis.max - y.axis.min) > 100) {
    # round up/down to next 50
    y.axis.max <- round_custom(y.axis.max, 50, 1)
    y.axis.min <- round_custom(y.axis.min, 50, 0)
  } else if (c(y.axis.max - y.axis.min) < 100 & c(y.axis.max - y.axis.min) > 50) {
    # round up/down to next 10
    y.axis.max <- round_custom(y.axis.max, 10, 1)
    y.axis.min <- round_custom(y.axis.min, 10, 0)
  } else if (c(y.axis.max - y.axis.min) < 50 & c(y.axis.max - y.axis.min) > 1) {
    # round to next 1
    y.axis.max <- round_custom(y.axis.max, 1, 1)
    y.axis.min <- round_custom(y.axis.min, 1, 0)
  } else if (c(y.axis.max - y.axis.min) < 1 & c(y.axis.max - y.axis.min) > 0.1) {
    # round to next 1
    y.axis.max <- round_custom(y.axis.max, 0.1, 1)
    y.axis.min <- round_custom(y.axis.min, 0.1, 0)
  } else if (c(y.axis.max - y.axis.min) < 0.1 & c(y.axis.max - y.axis.min) > 0.01) {
    # round to next 1
    y.axis.max <- round_custom(y.axis.max, 0.01, 1)
    y.axis.min <- round_custom(y.axis.min, 0.01, 0)
  } else if (c(y.axis.max - y.axis.min) < 0.01 & c(y.axis.max - y.axis.min) > 0.001) {
    # round to next 1
    y.axis.max <- round_custom(y.axis.max, 0.001, 1)
    y.axis.min <- round_custom(y.axis.min, 0.001, 0)
  }
  return(c(y.axis.min, y.axis.max))
}



# color scale
rgb2col <- function(rgbmat) {
  # function to apply to each column of input rgbmat
  ProcessColumn <- function(col) {
    rgb(rgbmat[1, col],
      rgbmat[2, col],
      rgbmat[3, col],
      maxColorValue = 255
    )
  }
  # Apply the function
  sapply(1:ncol(rgbmat), ProcessColumn)
}


# compress csv
compress_csv <- function(csv_file) {
  tmp <- readr::read_table(csv_file)

  new.name <- paste0(csv_file, ".gz")

  crunch::write.csv.gz(
    x = tmp,
    quote = FALSE,
    file = new.name
  )

  rm(list = ls())
  invisible(gc())
}

robust_spatial_trend <- function(x) {
  # x<-r.rast[[1]]

  r.time <- as.Date(terra::time(x))

  r.year <- as.numeric(format(r.time, "%Y"))

  r.df <- terra::as.data.frame(x, xy = TRUE)

  r.subtrend <- apply(r.df[, -c(1, 2)], c(1), function(y) {
    # test
    # y<-unlist(r.df[1,-c(1,2)])

    res <- robustbase::lmrob(y ~ r.time)

    return(cbind(coef(res), confint(res, level = 0.95)))
  })

  r.subtrend <- cbind.data.frame(
    variables = c(
      "Intercept", "slope",
      "Intercept2.5", "slope2.5",
      "Intercept97.5", "slope97.5"
    ),
    r.subtrend
  )

  # test
  r.subtrend <- tidyr::pivot_longer(as.data.frame(r.subtrend),
    cols = -"variables",
    names_to = "pixels"
  )

  r.crd <- cbind(
    pixels = rownames(r.df),
    r.df[, c(1, 2)]
  )


  r.subtrend <- dplyr::left_join(r.subtrend, r.crd, "pixels")

  r.subtrend <- r.subtrend[, c("x", "y", "variables", "value")]


  return(r.subtrend)
}


# clacuate mean, min, max for plotting
linear_trend_df <- function(x) {
  # x<-r.rast

  r.time <- as.Date(terra::time(x))

  r.array <- terra::as.array(x)

  r.mean <- apply(r.array, c(3), mean, na.rm = T)
  r.max <- apply(r.array, c(3), max, na.rm = T)
  r.min <- apply(r.array, c(3), min, na.rm = T)


  r.df <- data.frame(
    YEAR = r.time,
    Kmean = r.mean,
    Kmax = r.max,
    Kmin = r.min
  )
  return(r.df)
}
# clacuate mean, min, max for plotting
linear_trend_df_IDcol <- function(x, IDcol) {
  # x<-r.rast

  r.time <- as.Date(terra::time(x))

  r.array <- terra::as.array(x)

  r.mean <- apply(r.array, c(3), mean, na.rm = T)
  r.max <- apply(r.array, c(3), max, na.rm = T)
  r.min <- apply(r.array, c(3), min, na.rm = T)


  r.df <- data.frame(
    YEAR = r.time,
    Kmean = r.mean,
    Kmax = r.max,
    Kmin = r.min,
    ID = IDcol
  )
  return(r.df)
}

# boxplots_df
boxplot_df <- function(x) {
  # x<-r.rast[[1]]

  r.time <- as.Date(terra::time(x))

  periods <- list(
    period1 = which(r.time > as.Date("1970-12-31") &
      r.time < as.Date("2000-02-01")),
    period2 = which(r.time > as.Date("1990-12-31") &
      r.time < as.Date("2020-02-01")),
    period3 = which(r.time > as.Date("2020-12-31") &
      r.time < as.Date("2050-02-01")),
    period4 = which(r.time > as.Date("2069-12-31") &
      r.time < as.Date("2099-02-01"))
  )

  r.periods <- lapply(X = periods, FUN = function(y) {
    # y<-periods[[1]]
    terra::subset(x = x, subset = y) %>%
      terra::values(
        mat = F,
        dataframe = F,
        na.rm = T
      )
  })

  rm(periods)

  # https://stackoverflow.com/a/53969052/13818750
  r.periods <- Reduce(cbind.data.frame, r.periods)

  r.periods$ID <- 1:nrow(r.periods)

  colnames(r.periods) <- c(
    "1971-2000",
    "1991-2020",
    "2021-2050",
    "2070-2099",
    "ID"
  )
  r.periods <- tidyr::pivot_longer(r.periods,
    cols = -c("ID"),
    names_to = "Period",
    values_to = "Kvalue"
  )
  return(r.periods)
}
# yearly_boxplots_df
yearly_boxplot_df <- function(x) {
  # x<-r.rast[[1]]

  r.df <- terra::as.data.frame(x)

  r.time <- as.Date(terra::time(x))

  colnames(r.df) <- lubridate::year(r.time)

  r.df$ID <- 1:nrow(r.df)

  r.df <- tidyr::pivot_longer(r.df,
    cols = -c("ID"),
    names_to = "Period",
    values_to = "Kvalue"
  )
  return(r.df)
}

lm_fun <- function(x, x.time) {
  if (all(is.na(x))) {
    lm_coeff <- rep(NA, 7)
  } else {
    res <- lm(x ~ x.time)

    lm_coeff <-
      c(
        coefficients(res),
        summary(res)$coefficients[2, 4],
        confint(res, level = 0.95)
      )
  }

  return(unname(lm_coeff))
}

linear_spatial_trend <- function(x) {
  # x<-r.rast[[1]]

  r.time <- as.Date(terra::time(x))

  r.year <- as.numeric(format(r.time, "%Y"))

  r.subtrend <- terra::app(x, lm_fun, x.time = r.time)

  names(r.subtrend) <- c(
    "Intercept", "Slope",
    "p-value",
    "Intercept2.5", "Slope2.5",
    "Intercept97.5", "Slope97.5"
  )

  r.subtrend <- terra::as.data.frame(r.subtrend, xy = TRUE)

  # test
  r.subtrend <- tidyr::pivot_longer(as.data.frame(r.subtrend),
    cols = -c("x", "y"),
    names_to = "variables",
    values_to = "Kvalue"
  )


  return(r.subtrend)

  # test
  # test.df<-read.csv(file = "~/Downloads/data-marketing-budget-12mo.csv", header=T,
  #                   colClasses = c("numeric", "numeric", "numeric"))
  #
  # res = lm(Sales~Spend, data=test.df)
  # summary(res)
  # multi.fit = lm(Sales~Spend+Month, data=test.df)
  # summary(multi.fit)
}
# calculate the mean for four periods
mean_four_periods <- function(x) {
  # x<-r.rast[[1]]

  r.time <- as.Date(terra::time(x))

  periods <- list(
    period1 = which(r.time > as.Date("1970-12-31") &
      r.time < as.Date("2000-02-01")),
    period2 = which(r.time > as.Date("1990-12-31") &
      r.time < as.Date("2020-02-01")),
    period3 = which(r.time > as.Date("2020-12-31") &
      r.time < as.Date("2050-02-01")),
    period4 = which(r.time > as.Date("2069-12-31") &
      r.time < as.Date("2099-02-01"))
  )

  r.periods <- lapply(X = periods, FUN = function(y) {
    # y<-periods[[1]]
    terra::subset(x = x, subset = y) %>%
      terra::mean()
  })

  rm(periods)

  # https://stackoverflow.com/a/53969052/13818750
  r.periods <- lapply(r.periods, terra::as.data.frame, xy = TRUE) %>%
    purrr::imap(.x = ., ~ purrr::set_names(.x, c("x", "y", .y))) %>%
    purrr::reduce(dplyr::left_join, by = c("x", "y")) %>%
    tidyr::pivot_longer(
      cols = -c("x", "y"),
      names_to = "Periods"
    )

  return(r.periods)
}
# calculate the mean and relative change for four periods
relative_change_four_periods <- function(x) {
  # x<-r.rast[[1]]

  r.time <- as.Date(terra::time(x))

  periods <- list(
    period1 = which(r.time > as.Date("1970-12-31") &
      r.time < as.Date("2000-02-01")),
    period2 = which(r.time > as.Date("1990-12-31") &
      r.time < as.Date("2020-02-01")),
    period3 = which(r.time > as.Date("2020-12-31") &
      r.time < as.Date("2050-02-01")),
    period4 = which(r.time > as.Date("2069-12-31") &
      r.time < as.Date("2099-02-01"))
  )

  r.periods <- lapply(X = periods, FUN = function(y) {
    # y<-periods[[1]]
    terra::subset(x = x, subset = y) %>%
      terra::mean()
  })

  rm(periods)
  r.periods$period2 <- r.periods$period2 - r.periods$period1
  r.periods$period3 <- r.periods$period3 - r.periods$period1
  r.periods$period4 <- r.periods$period4 - r.periods$period1

  # https://stackoverflow.com/a/53969052/13818750
  r.periods <- lapply(r.periods, terra::as.data.frame, xy = TRUE) %>%
    purrr::imap(.x = ., ~ purrr::set_names(.x, c("x", "y", .y))) %>%
    purrr::reduce(dplyr::left_join, by = c("x", "y")) %>%
    tidyr::pivot_longer(
      cols = -c("x", "y"),
      names_to = "Periods"
    )

  return(r.periods)
}

# heat maps from all ennsemble members
heatmaps_5vars_3RCP <- function(x) {
  # x<-r.rast[[1]]
  # x<- terra::rast(netCDF.files[1])

  r.time <- as.Date(terra::time(x))

  r.mean <- vector(length = terra::nlyr(x))

  for (ilyr in 1:terra::nlyr(x)) {
    r.mean[ilyr] <- terra::global(x[[ilyr]], fun = "mean", na.rm = T)
  }
  r.mean <- unlist(r.mean)
  # old approach, not time consuming rather memory
  # r.mean <- apply(terra::as.array(x), MARGIN = c(3), mean, na.rm = T)

  r.names <- stringr::str_to_title(sub("_.*", "", names(x)))

  r.df <- data.frame(
    YEAR = r.time,
    Ensemble = r.names,
    Kvalue = r.mean
  )

  return(r.df)
}


# boxplots_df
pdf_df <- function(x) {
  # x<-r.rast[[1]]

  r.time <- as.Date(terra::time(x))

  periods <- list(
    period1 = which(r.time > as.Date("1970-12-31") &
      r.time < as.Date("2000-02-01")),
    period2 = which(r.time > as.Date("1990-12-31") &
      r.time < as.Date("2020-02-01")),
    period3 = which(r.time > as.Date("2020-12-31") &
      r.time < as.Date("2050-02-01")),
    period4 = which(r.time > as.Date("2069-12-31") &
      r.time < as.Date("2099-02-01"))
  )

  r.periods <- lapply(X = periods, FUN = function(y) {
    # y<-periods[[1]]
    terra::subset(x = x, subset = y) %>%
      terra::values(
        mat = F,
        dataframe = F,
        na.rm = T
      )
  })

  rm(periods)

  # https://stackoverflow.com/a/53969052/13818750
  r.periods <- Reduce(cbind.data.frame, r.periods)
  colnames(r.periods) <- c(
    "1971-2000",
    "1991-2020",
    "2021-2050",
    "2070-2099"
  )

  r.means <- colMeans(r.periods)
  r.sd <- sapply(r.periods, sd)
  r.x.axis <- seq(0, 500, 1)

  r.result <- data.frame(x = r.x.axis)

  r.pdf <- lapply(1:4, function(x) {
    dnorm(x = r.x.axis, mean = r.means[x], sd = r.sd[x])
  })


  r.pdf <- Reduce(cbind.data.frame, r.pdf)

  names(r.pdf) <- c(
    "1971-2000",
    "1991-2020",
    "2021-2050",
    "2070-2099"
  )


  r.pdf$Kvar <- r.x.axis


  r.pdf <- tidyr::pivot_longer(r.pdf,
    cols = -c("Kvar"),
    names_to = "Period",
    values_to = "Kvalue"
  )
  return(r.pdf)
}

# colors ------------------------------------------------------------------

rcps_colours_temp <- function(n) {
  rcpcolors <- matrix(c(
    103, 0, 31,
    178, 24, 43,
    214, 96, 77,
    244, 165, 130,
    253, 219, 199,
    247, 247, 247,
    209, 229, 240,
    146, 197, 222,
    67, 147, 195,
    33, 102, 172
  ), byrow = F, nrow = 3)

  rcps_colours <- grDevices::colorRampPalette(rgb2col(rcpcolors))

  return(rcps_colours(n))
}


rcps_colours_precip <- function(n) {
  rcpcolors <- matrix(c(
    084, 048, 005,
    140, 081, 010,
    191, 129, 045,
    223, 194, 125,
    246, 232, 195,
    245, 245, 245,
    199, 234, 229,
    128, 205, 193,
    053, 151, 143,
    001, 102, 094,
    000, 060, 048
  ), byrow = F, nrow = 3)

  rcps_colours <- grDevices::colorRampPalette(rgb2col(rcpcolors))


  return(rcps_colours(n))
}

month.abb.DE <- c(
  "Jan", "Feb", "Mrz",
  "Apr", "Mai", "Jun",
  "Jul", "Aug", "Sep",
  "Okt", "Nov", "Dez"
)

labels_replace <- function(x) {
  stringr::str_replace(
    pattern = "Precip",
    replacement = "N",
    x
  )
}


set_sec_axis <- function(y.prim, y.sec.range) {
  # y.sec.range<-range(data$Precip)
  # y.prim<-ylim.prim

  y.sec.range <- setting_nice_limits(y.sec.range[1], y.sec.range[2])
  b <- 1
  left <- y.prim[1] * b
  right <- y.prim[2] * b

  while (y.sec.range[1] < left | y.sec.range[2] > right) {
    left <- y.prim[1] * b
    right <- y.prim[2] * b

    b <- b + 0.1
  }

  return(c(left, right))
}

Heliothermal_Index <- function(tmax,
                               tmean,
                               k = 1.064,
                               dates) {
  # test<-hi_res[hi_res$YEAR==1882,]
  # tmax<-test$tmax; tmean = test$tmean
  # dates<-test$Datum

  #
  iniday <- "04-01"
  endday <- "09-30"

  days <- dates[which(as.numeric(substr(dates, 6, 7))
  %in%
    substr(iniday, 1, 2):substr(endday, 1, 2))]

  HI <- sum(k * (0.5 * ((tmax[dates %in% days] - 10) + (tmean[dates %in% days] - 10))),
    na.rm = T
  )
  return(HI)
}
