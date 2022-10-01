

# mean of SpatRaster
f1_mean <- function(x) {
  y <- terra::as.array(x) %>%
    apply(MARGIN = c(3), FUN = mean, na.rm = TRUE)

  return(y)
}



# max of spatRaster in vector forum
f1_max <- function(x) {
  y <- terra::as.array(x) %>%
    apply(MARGIN = c(3), FUN = max, na.rm = TRUE)

  return(y)
}

# min
f1_min <- function(x) {
  y <- terra::as.array(x) %>%
    apply(MARGIN = c(3), FUN = min, na.rm = TRUE)

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
  gc()
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

lm_fun <- function(x, x.time) {

  if(all(is.na(x))){

    lm_coeff<-rep(NA,7)

  }else{

    res <- lm(x ~ x.time)

    lm_coeff<-
      c(coefficients(res),
        summary(res)$coefficients[2,4],
        confint(res, level = 0.95))

  }

  return(unname(lm_coeff))

}

linear_spatial_trend <- function(x) {

  # x<-r.rast[[1]]

  r.time <- as.Date(terra::time(x))

  r.year <- as.numeric(format(r.time, "%Y"))

  r.subtrend <- terra::app(x, lm_fun, x.time =r.time)

  names(r.subtrend)<-c(
    "Intercept", "Slope",
    "p-value",
    "Intercept2.5", "Slope2.5",
    "Intercept97.5", "Slope97.5"
  )

  r.subtrend <- terra::as.data.frame(r.subtrend, xy = TRUE)

  # test
  r.subtrend <- tidyr::pivot_longer(as.data.frame(r.subtrend),
    cols = -c("x","y" ),
    names_to = "variables",
    values_to = "Kvalue"
  )


  return(r.subtrend)
}

# calculate the mean for four periods
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



# complete a df of expnded grid
complete_df_of_expanded_grid <- function(x) {


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



# internal data -----------------------------------------------------------

Bundeslaender<-  utils::data("Bundeslaender",
                             envir = environment())
Landkreise<-utils::data("Landkreise",
                        envir = environment())
land_cover<-utils::data("land_cover",
                        envir = environment())

standard_output_de<- utils::data("standard_output_de",
                                 envir = environment()
)
standard_output_en<- utils::data("standard_output_en",
                                 envir = environment()
)
land_cover_legend<-utils::data("land_cover_legend",
                               envir = environment())
# creat robust lm
# library(robustbase)
# res <- lmrob(light ~ temperature,
#              data=currentDataset)
# summary(res)
# cbind(coef(res),confint(res, level = 0.95))
