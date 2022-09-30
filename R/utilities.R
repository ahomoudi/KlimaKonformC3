

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

linear_spatial_trend <- function(x) {

  # x<-r.rast[[1]]

  r.time <- as.Date(terra::time(x))

  r.year <- as.numeric(format(r.time, "%Y"))

  r.df <- terra::as.data.frame(x, xy = TRUE)

  r.subtrend <- apply(r.df[, -c(1, 2)], c(1), function(y) {

    # test
    # y<-unlist(r.df[1,-c(1,2)])

    res <- lm(y ~ r.time)
    # res<-robustbase::lmrob(y~r.time)

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

# complete a df of expnded grid
complete_df_of_expanded_grid <- function(x) {


}
# creat robust lm
# library(robustbase)
# res <- lmrob(light ~ temperature,
#              data=currentDataset)
# summary(res)
# cbind(coef(res),confint(res, level = 0.95))
