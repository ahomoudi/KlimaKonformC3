

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
setting_nice_intervals <- function(minval, maxval) {
  diff <- maxval - minval

  intervals <- diff %/% 8 # setting 8 as 10 labels minus two (limits)


  leg_breaks <- seq.default(
    from = minval,
    to = maxval,
    by = intervals
  )

  # while (length(scaleFUN(leg_breaks)) != length(unique(scaleFUN(leg_breaks)))) {
  #
  #   breaks_length <- breaks_length - 1
  #
  #   if (breaks_length <= 4) {
  #     break
  #   }
  #
  #   leg_breaks <- seq.default(
  #     from = opt_min,
  #     to = opt_max,
  #     length.out = breaks_length
  #   )
  # }
  return(leg_breaks)
}
