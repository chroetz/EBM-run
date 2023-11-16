.PopWeightAggregationEnv <- new.env()

#' @export
setupPopWeightAggregation <- function(
  degStep,
  countryMaskPath,
  popDir,
  popFileNamePattern,
  invarDir,
  invarFileNamePattern,
  invarDimensionName,
  invarValueVariableName,
  outDir,
  outNcFilePattern
) {
  argNames <- rlang::fn_fmls_names()
  env <- rlang::current_env()
  lapply(
    argNames,
    \(nm) assign(nm, env[[nm]], .PopWeightAggregationEnv)
  )

  .PopWeightAggregationEnv$eps <- sqrt(.Machine$double.eps)

  .PopWeightAggregationEnv$grid <- list(
    lonValues = seq(-180, 180, by = degStep)[-1] - degStep/2,
    latValues = seq(-90, 90, by = degStep)[-1] - degStep/2)

  popFileNames <- list.files(popDir, pattern=popFileNamePattern)
  fileYears <- stringr::str_match(popFileNames, popFileNamePattern)[,2] |> as.integer()
  .PopWeightAggregationEnv$popFileMeta <- tibble(
    year = fileYears,
    name = popFileNames,
    path = file.path(popDir, popFileNames))

  invarFileNames <- list.files(
    invarDir,
    pattern = invarFileNamePattern)
  fileYears <- stringr::str_match(invarFileNames, invarFileNamePattern)[,2] |> as.integer()
  .PopWeightAggregationEnv$invarFileMeta <- tibble(
    year = fileYears,
    name = invarFileNames,
    path = file.path(invarDir, invarFileNames))
  return(invisible())
}


#' @export
setupPopWeightAggregationStatistics <- function(...) {
  args <- list(...)
  .PopWeightAggregationEnv$statisticNames <- names(args)
  .PopWeightAggregationEnv$statisticFunctions <- args
  return(invisible())
}


#' @export
runPopWeightAggregation <- function(yearsFilter = NULL) {
  cat("Get years ... ")
  years <- getYears()
  if (!is.null(yearsFilter)) years <- intersect(years, yearsFilter)
  cat(length(years), "years to process.\n")

  cat("Get regions ... ")
  regionNames <- getRegionNames()
  cat(length(regionNames), "regions to process.\n")

  cat("Initializing output files... ")
  initOutNc(years, regionNames, .PopWeightAggregationEnv$statisticNames)
  cat("Done.\n")

  cat("Start main loop.\n")
  for (year in years) {
    cat("Year:", year, "\n")
    popValues <- getPopValues(year)
    invarNames <- getInvarNames(year)
    for (regionName in regionNames) {
      cat("\tRegion:", regionName, "\n")
      maskValues <- getMaskValues(regionName)
      popRegionDistri <- calcPopRegionDistri(popValues, maskValues)
      pt <- proc.time()
      processRegionYear(regionName, year, invarNames, popRegionDistri)
      cat("\tprocessRegionYear duration:", (proc.time()-pt)[3], "s\n")
    }
  }
  cat("End main loop.\n")
}


processRegionYear <- function(regionName, year, invarNames, popRegionDistri) {
  for (invarName in invarNames) {
    cat("\t\tVariable:", invarName, "\n")
    pt <- proc.time()
    invarValues <- getInvarValues(year, invarName)
    cat("getInvarValues():")
    print(proc.time()-pt)
    for (statisticName in .PopWeightAggregationEnv$statisticNames) {
      cat("\t\t\tStatistic:", statisticName, "...")
      pt <- proc.time()
      x <- claculateStatisticOnGrid(statisticName, invarValues)
      result <- integrateDistribution(popRegionDistri, x)
      cat("calculate:")
      print(proc.time()-pt)
      pt <- proc.time()
      saveResult(result, year, regionName, statisticName, invarName)
      cat("save:")
      print(proc.time()-pt)
      cat(" Done.\n")
    }
  }
}


assertLonLat <- function(lonValues, latValues) {
  stopifnot(
    max(abs(.PopWeightAggregationEnv$grid$latValues - latValues)) < .PopWeightAggregationEnv$eps,
    max(abs(.PopWeightAggregationEnv$grid$lonValues - lonValues)) < .PopWeightAggregationEnv$eps)

}

claculateStatisticOnGrid <- function(statisticName, invarValues) {
  .PopWeightAggregationEnv$statisticFunctions[[statisticName]](invarValues)
}

getRegionNames <- function() {
  nc <- open.nc(.PopWeightAggregationEnv$countryMaskPath)
  varNames <- ncGetNonDimVariableNames(nc)
  close.nc(nc)
  return(varNames)
}

getPopValues <- function(year) {
  fileInfo <-
    .PopWeightAggregationEnv$popFileMeta |>
    filter(.data$year == .env$year)
  stopifnot(nrow(fileInfo) == 1)
  pop <- list()
  nc <- open.nc(fileInfo$path)
  pop$lonValues <- var.get.nc(nc, "lon")
  pop$latValues <- var.get.nc(nc, "lat")
  pop$values <- var.get.nc(nc, "total-population")
  close.nc(nc)

  assertLonLat(pop$lonValues, pop$latValues)

  popValues <- ifelse(is.na(pop$values), 0, pop$values)

  return(popValues)
}

getInvarFilePath <- function(year) {
  fileInfo <- .PopWeightAggregationEnv$invarFileMeta |> filter(.data$year == .env$year)
  stopifnot(nrow(fileInfo) == 1)
  return(fileInfo$path)
}

getInvarNames <- function(year) {
  filePath <- getInvarFilePath(year)

  nc <- open.nc(filePath)
  invarNames <- var.get.nc(nc, .PopWeightAggregationEnv$invarDimensionName)
  close.nc(nc)

  return(invarNames)
}

reverseArrayDim <- function(x, i) {
  DescTools::Rev(x, i)
}

getMaskValues <- function(regionName) {
  mask <- list()
  nc <- open.nc(.PopWeightAggregationEnv$countryMaskPath)
  latIdx <- ncGetDimensionIndex(nc, "lat")
  mask$lonValues <- var.get.nc(nc, "lon")
  mask$latValues <- var.get.nc(nc, "lat")
  mask$values <- var.get.nc(nc, regionName)
  close.nc(nc)

  assertLonLat(mask$lonValues, rev(mask$latValues))

  maskValues <- reverseArrayDim(mask$values, latIdx)

  return(maskValues)
}

calcPopRegionDistri <- function(popValues, maskValues) {
  stopifnot(identical(dim(maskValues), dim(popValues)))
  maskPop <- maskValues * popValues
  maskPopDistri <- maskPop / sum(maskPop)
  return(maskPopDistri)
}

getInvarValues <- function(year, invarName) {
  filePath <- getInvarFilePath(year)
  invar <- list()
  nc <- open.nc(filePath)
  invar$lonValues <- var.get.nc(nc, "lon")
  invar$latValues <- var.get.nc(nc, "lat")
  invar$dimensionValues <- var.get.nc(nc, .PopWeightAggregationEnv$invarDimensionName)
  invarIdx <- which(invar$dimensionValues == invarName)
  invar$values <- var.get.nc(
    nc,
    .PopWeightAggregationEnv$invarValueVariableName,
    start = c(1, 1, invarIdx),
    count = c(NA, NA, 1))
  close.nc(nc)

  assertLonLat(invar$lonValues, invar$latValues)

  return(invar$values)
}

integrateDistribution <- function(distri, values) {
  stopifnot(identical(dim(distri), dim(values)))
  integral <- sum(distri * values)
  return(integral)
}

getOutNcFilePath <- function(year) {
  file.path(
    .PopWeightAggregationEnv$outDir,
    sprintf(.PopWeightAggregationEnv$outNcFilePattern, year))
}

initOutNc <- function(years, regionNames, statisticNames) {
  for (year in years) {
    outNcFilePath <- getOutNcFilePath(year)
    if (file.exists(outNcFilePath)) {
      warning(outNcFilePath, " already exits. Skipping.")
      next
    }
    outNc <- create.nc(outNcFilePath, format = "netcdf4")
    dim.def.nc(outNc, "region", dimlength = length(regionNames))
    var.def.nc(outNc, "region", "NC_STRING", "region")
    var.put.nc(outNc, "region", regionNames)
    dim.def.nc(outNc, "statistic", dimlength = length(statisticNames))
    var.def.nc(outNc, "statistic", "NC_STRING", "statistic")
    var.put.nc(outNc, "statistic", statisticNames)
    close.nc(outNc)
  }
}



saveResult <- function(result, year, regionName, statisticName, variableName) {
  outNcFilePath <- getOutNcFilePath(year)
  outNc <- open.nc(outNcFilePath, write = TRUE)
  if (!ncHasVariable(outNc, variableName)) {
    var.def.nc(outNc, variableName, "NC_DOUBLE", c("region", "statistic"), deflate = 9)
  }
  regionNames <- var.get.nc(outNc, "region")
  regionIdx <- which(regionName == regionNames)
  stopifnot(length(regionIdx) == 1)
  statisticNames <- var.get.nc(outNc, "statistic")
  statisticIdx <- which(statisticName == statisticNames)
  stopifnot(length(statisticIdx) == 1)
  var.put.nc(
    outNc,
    variableName,
    result,
    start = c(regionIdx, statisticIdx),
    count = c(1, 1))
  close.nc(outNc)
}

getYears <- function() {
  intersect(
    .PopWeightAggregationEnv$popFileMeta$year,
    .PopWeightAggregationEnv$invarFileMeta$year)
}
