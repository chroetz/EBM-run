loadData <- function(name, dataDescriptor) {
  switch(
    dataDescriptor$type,
    YearlyFiles = loadDataYearlyFiles(name, dataDescriptor),
    SingleFile = loadDataSingleFile(name, dataDescriptor),
    stop("Unknown data descriptor type: ", dataDescriptor$type)
  )
  return(invisible())
}

loadDataYearlyFiles <- function(name, dataDescriptor) {
  fileNames <- list.files(dataDescriptor$dir, pattern=dataDescriptor$pattern)
  fileYears <- stringr::str_match(fileNames, dataDescriptor$pattern)[,2] |> as.integer()
  filePaths <- file.path(dataDescriptor$dir, fileNames)

  lonLatCheck <- checkLonLatInNcFile(filePaths[1])

  if (!"data" %in% names(.info)) .info$data <- list()
  .info$data[[name]] <- list(
    name = name,
    descriptor = dataDescriptor,
    lonIncreasing = lonLatCheck$lonIncreasing,
    latIncreasing = lonLatCheck$latIncreasing,
    years = fileYears,
    meta = tibble(
      year = fileYears,
      name = fileNames,
      path = filePaths))
}

loadDataSingleFile <- function(name, dataDescriptor) {
  lonLatCheck <- checkLonLatInNcFile(dataDescriptor$filePath)

  nc <- open.nc(dataDescriptor$filePath)
  timeDimName <- setdiff(ncGetDimensionNames(nc), c("lon", "lat"))
  stopfifnot(length(timeDimName) == 1)
  timeValues <- var.get.nc(nc, timeDimName)
  timeUnitDescription <- att.get.nc(nc, timeDimName, "units")
  close.nc(nc)

  startDayText <- str_match(timeUnitDescription, "^days since ([\\d-]+)( \\d{2}:\\d{2}:(\\d{2})?)?")[,2]
  startYear <- startDayText |> as.Date() |> lubridate::year()
  years <- timeValues/365 + startYear

  stopifnot(max(abs(years - round(years))) > sqrt(.Machine$double.eps))

  if (!"data" %in% names(.info)) .info$data <- list()
  .info$data[[name]] <- list(
    name = name,
    descriptor = dataDescriptor,
    lonIncreasing = lonLatCheck$lonIncreasing,
    latIncreasing = lonLatCheck$latIncreasing,
    years = years,
    timeDimName = timeDimName,
    filePath = dataDescriptor$filePath)
}


checkLonLatInNcFile <- function(filePath) {
  nc <- open.nc(filePath)
  dimNames <- ncGetDimensionNames(nc)
  close.nc(nc)
  stopifnot(c("lon", "lat") %in% dimNames)
  lonValues <- var.get.nc(nc, "lon")
  latValues <- var.get.nc(nc, "lat")
  if (all(diff(lonValues) > 0)) lonIncreasing <- TRUE
  else if (all(diff(lonValues) < 0)) lonIncreasing <- FALSE
  else stop("Longitude values are neither increasing nor decreasing.")
  if (all(diff(latValues) > 0)) latIncreasing <- TRUE
  else if (all(diff(latValues) < 0)) latIncreasing <- FALSE
  else stop("Latitude values are neither increasing nor decreasing.")
  assertLonLat(
    if (lonIncreasing) lonValues else rev(lonValues),
    if (latIncreasing) latValues else rev(latValues))
  return(lst(lonIncreasing, latIncreasing))
}
