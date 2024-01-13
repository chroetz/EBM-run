#' GridFormat class
#' @description A class to store the format of a longitude - latitude grid of the globe.
#' @param degStep The degree step of the grid.
#' @param lonFirst Whether the longitude is the first dimension.
#' @param lonIncreasing Whether the longitude values are in increasing order.
#' @param latIncreasing Whether the latitude values are in increasing order.
#' @examples
#' createGridFormat(1, TRUE, TRUE, TRUE)
#' @export
createGridFormat <- function(
    degStep,
    lonFirst,
    lonIncreasing,
    latIncreasing
) {
  lst <- list(
      degStep = degStep,
      lonFirst = lonFirst,
      lonIncreasing = lonIncreasing,
      latIncreasing = latIncreasing)
  class(lst) <- "GridFormat"
  return(lst)
}

getLonValues <- function(degStep) {
  v <- getLonLatValues(degStep, -180, 180)
  return(v)
}

getLatValues <- function(degStep) {
  v <- getLonLatValues(degStep, -90, 90)
  return(v)
}

getLonLatValues <- function(degStep, from, to) {
  v <- seq(from, to, by = degStep)[-1] - degStep/2
  return(v)
}

format.GridFormat <- function(x, ...) {
  nLonVals <- getLonValues(x$degStep) |> length()
  nLatVals <- getLatValues(x$degStep) |> length()
  paste0(
    "GridFormat:\n",
    "\tstep size ", x$degStep, " degrees\n",
    "\tlon from -180 to 180 (", nLonVals, " values)\n",
    "\tlat from -90 to 90 (", nLatVals, " values)\n",
    "\torder:", if (x$lonFirst) "lon x lat" else "lat x lon", "\n",
    "\tlon in ", if (x$lonIncreasing) "increasing" else "decreasing", " order\n",
    "\tlat in ", if (x$latIncreasing) "increasing" else "decreasing", " order")
}


print.GridFormat <- function(x, ...) {
  cat(format(x), "\n")
}


initializeGrid <- function(targetFormat) {
  .info$gridTol <- targetFormat$degStep / 10
  .info$grid <- c(
    targetFormat,
    list(
      increasingLonValues = getLonValues(targetFormat$degStep),
      increasingLatValues = getLatValues(targetFormat$degStep),
      orderedNames = if (targetFormat$lonFirst) c("lon", "lat") else c("lat", "lon")
    )
  )
  if (targetFormat$lonIncreasing) {
    .info$grid$lonValues <- .info$grid$increasingLonValues
  } else {
    .info$grid$lonValues <- .info$grid$increasingLonValues |> rev()
  }
  if (targetFormat$latIncreasing) {
    .info$grid$latValues <- .info$grid$increasingLatValues
  } else {
    .info$grid$latValues <- .info$grid$increasingLatValues |> rev()
  }
  cat("Target Grid:", format(targetFormat), "\n")
}

