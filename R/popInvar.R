.global <- new.env()

#' @export
setupPopWeightAggregation <- function(
  degStep,
  countryMaskPath,
  boundingBoxPath,
  popDir,
  popFileNamePattern,
  invarDir,
  invarFileNamePattern,
  invarDimensionName,
  invarValueVariableName,
  outDir,
  outNcFilePattern,
  batchSize
) {
  argNames <- rlang::fn_fmls_names()
  env <- rlang::current_env()
  lapply(
    argNames,
    \(nm) assign(nm, env[[nm]], .global)
  )

  .global$eps <- sqrt(.Machine$double.eps)

  .global$grid <- list(
    lonValues = seq(-180, 180, by = degStep)[-1] - degStep/2,
    latValues = seq(-90, 90, by = degStep)[-1] - degStep/2)

  .global$idxBoundingBoxes <- readr::read_csv(boundingBoxPath, col_types = readr::cols())

  popFileNames <- list.files(popDir, pattern=popFileNamePattern)
  fileYears <- stringr::str_match(popFileNames, popFileNamePattern)[,2] |> as.integer()
  .global$popFileMeta <- tibble(
    year = fileYears,
    name = popFileNames,
    path = file.path(popDir, popFileNames))

  invarFileNames <- list.files(
    invarDir,
    pattern = invarFileNamePattern)
  fileYears <- stringr::str_match(invarFileNames, invarFileNamePattern)[,2] |> as.integer()
  .global$invarFileMeta <- tibble(
    year = fileYears,
    name = invarFileNames,
    path = file.path(invarDir, invarFileNames))
  return(invisible())
}


#' @export
setupPopWeightAggregationStatistics <- function(...) {
  args <- list(...)
  .global$statisticNames <- names(args)
  .global$statisticFunctions <- args
  return(invisible())
}


#' @export
runPopWeightAggregation <- function(yearsFilter = NULL, invarNamesIdxFilter = NULL) {
  cat("Get years ... ")
  years <- getYears()
  if (!is.null(yearsFilter)) years <- intersect(years, yearsFilter)
  cat(length(years), "years to process.\n")

  cat("Get regions ... ")
  regionNames <- getRegionNames()
  cat(length(regionNames), "regions to process.\n")

  cat("Initializing output files...\n")
  initOutNc(years, regionNames, .global$statisticNames)
  cat("Done.\n")

  cat("Start main loop.\n")
  for (year in years) {
    cat("Year:", year, "\n")
    popValuesAll <- getPopValues(year)
    invarNames <- getInvarNames(year)
    if (!is.null(invarNamesIdxFilter)) {
      invarNames <- invarNames[invarNamesIdxFilter]
      invarNames <- invarNames[!is.na(invarNames)]
    }
    if (length(invarNames) == 0) {
      cat("No invarNames to process in year", year, ". Skipping.\n")
      next
    }
    checkInvar(year, invarNames)
    fullyFilledRegionNames <- getFullyFilledRegionNames(year, invarNames)
    if (length(fullyFilledRegionNames) > 0) {
      cat(
        "\tFound",
        length(fullyFilledRegionNames),
        "regions with data. Not re-calculating those.\n")
      regionNames <- setdiff(regionNames, fullyFilledRegionNames)
    } else {
      cat("\tNo filled regions found. Processing all.\n")
    }
    for (regionName in regionNames) {
      cat("\tRegion:", regionName, "\n")
      popValuesRegion <- subsetRegion(popValuesAll, regionName)
      maskValues <- getMaskValues(regionName)
      popRegionDistri <- calcPopRegionDistri(popValuesRegion, maskValues)
      pt <- proc.time()
      processRegionYear(
        regionName,
        year,
        invarNames,
        popRegionDistri,
        batchSize = .global$batchSize)
      cat("\tprocessRegionYear duration:", (proc.time()-pt)[3], "s\n")
      gc(verbose = FALSE)
    }
  }
  cat("End main loop.\n")
}

processRegionYear <- function(regionName, year, invarNames, popRegionDistri, batchSize) {
  stopifnot(batchSize >= 1)
  n <- length(invarNames)
  nBatches <- ceiling(n/batchSize)
  batchIdxs <- lapply(
    seq_len(nBatches),
    \(k) {
      idxs <- (1+(k-1)*batchSize):(k*batchSize)
      idxs[idxs <= n]
    }
  )
  for (batchNr in seq_len(nBatches)) {
    idxs <- batchIdxs[[batchNr]]
    cat("\t\tVariable indices from", min(idxs), "to", max(idxs), "\n")
    pt <- proc.time()
    invarValues <- getInvarValues(year, min(idxs), length(idxs), regionName) # this takes time
    cat("\t\t\tgetInvarValues() took", (proc.time()-pt)[3], "s\n")
    pt <- proc.time()
    for (statisticName in .global$statisticNames) {
      cat("\t\t\tStatistic:", statisticName, "...")
      x <- calculateStatisticOnGrid(statisticName, invarValues)
      results <- integrateDistribution(popRegionDistri, x)
      saveResult(results, year, regionName, statisticName, invarNames[idxs])
      cat(" Done.\n")
    }
    cat("\t\t\tcalculating and saving took", (proc.time()-pt)[3], "s\n")
  }
}


getFullyFilledRegionNames <- function(year, invarNames) {
  outNcFilePath <- getOutNcFilePath(year)
  outNc <- open.nc(outNcFilePath, share = FALSE)
  regionNames <- var.get.nc(outNc, "region")
  variableNames <- ncGetNonDimVariableNames(outNc)
  if (any(!invarNames %in% variableNames)) {
    return(NULL)
  }
  allData <- tryCatch(read.nc(outNc), error = \(cond) FALSE)
  close.nc(outNc)
  if (isFALSE(allData)) {
    stop("The file ", outNcFilePath, " is corrupt! Probably need to delete it and run calculations again.")
  }
  hasNa <- sapply(
    invarNames,
    \(invarName) rowSums(is.na(allData[[invarName]])) > 0)
  isRegionFilled <- rowSums(hasNa) == 0
  return(regionNames[isRegionFilled])
}


assertLonLat <- function(lonValues, latValues) {
  stopifnot(
    max(abs(.global$grid$latValues - latValues)) < .global$eps,
    max(abs(.global$grid$lonValues - lonValues)) < .global$eps)
}

calculateStatisticOnGrid <- function(statisticName, invarValues) {
  .global$statisticFunctions[[statisticName]](invarValues)
}

getRegionNames <- function() {
  nc <- open.nc(.global$countryMaskPath)
  varNames <- ncGetNonDimVariableNames(nc)
  close.nc(nc)
  return(varNames)
}

getPopValues <- function(year) {
  fileInfo <-
    .global$popFileMeta |>
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
  if (any(is.na(popValues))) {
    message("WARNING: NAs in population values in year ", year)
  }

  return(popValues)
}

subsetRegion <- function(valuesOnTotalGrid, regionName) {
  info <- .global$idxBoundingBoxes |> dplyr::filter(GID_1 == regionName) |> as.list()
  return(valuesOnTotalGrid[info$min_lon:info$max_lon, info$min_lat:info$max_lat, drop=FALSE])
}

getInvarFilePath <- function(year) {
  fileInfo <- .global$invarFileMeta |> filter(.data$year == .env$year)
  stopifnot(nrow(fileInfo) == 1)
  return(fileInfo$path)
}

getInvarNames <- function(year) {
  filePath <- getInvarFilePath(year)
  nc <- open.nc(filePath)
  invarNames <- var.get.nc(nc, .global$invarDimensionName)
  close.nc(nc)
  return(invarNames)
}

reverseArrayDim <- function(x, i) {
  DescTools::Rev(x, i)
}

getMaskValues <- function(regionName, onlyBoundingBox = TRUE) {
  mask <- list()
  nc <- open.nc(.global$countryMaskPath)
  latIdx <- ncGetDimensionIndex(nc, "lat")
  mask$lonValues <- var.get.nc(nc, "lon")
  mask$latValues <- var.get.nc(nc, "lat")
  if (onlyBoundingBox) {
    bbInfo <- .global$idxBoundingBoxes |> dplyr::filter(GID_1 == regionName) |> as.list()
    mask$values <- var.get.nc(
      nc,
      regionName,
      start = c(bbInfo$min_lon, bbInfo$min_lat),
      count = c(bbInfo$max_lon - bbInfo$min_lon + 1, bbInfo$max_lat - bbInfo$min_lat + 1),
      collapse = FALSE)
  } else {
    mask$values <- var.get.nc(nc, regionName)
  }
  close.nc(nc)

  assertLonLat(mask$lonValues, rev(mask$latValues))

  maskValues <- reverseArrayDim(mask$values, latIdx)
  if (any(is.na(maskValues))) {
    message("WARNING: NAs in mask values in region ", regionName)
  }

  return(maskValues)
}

calcPopRegionDistri <- function(popValues, maskValues) {
  stopifnot(identical(dim(maskValues), dim(popValues)))
  maskPop <- maskValues * popValues
  maskPopDistri <- maskPop / pmax(1, sum(maskPop))
  return(maskPopDistri)
}

checkInvar <- function(year, invarNames) {
  filePath <- getInvarFilePath(year)
  invar <- list()
  nc <- open.nc(filePath)
  invar$lonValues <- var.get.nc(nc, "lon")
  invar$latValues <- var.get.nc(nc, "lat")
  invar$dimensionValues <- var.get.nc(nc, .global$invarDimensionName)
  close.nc(nc)
  assertLonLat(invar$lonValues, invar$latValues)
  stopifnot(all(invarNames %in% invar$dimensionValues))
  return(invisible())
}

getInvarValues <- function(year, fromIdx, count, regionName) {
  bbInfo <- .global$idxBoundingBoxes |> dplyr::filter(GID_1 == regionName) |> as.list()
  filePath <- getInvarFilePath(year)
  nc <- open.nc(filePath)
  invarValues <- var.get.nc(
    nc,
    .global$invarValueVariableName,
    start = c(bbInfo$min_lon, bbInfo$min_lat, fromIdx),
    count = c(bbInfo$max_lon - bbInfo$min_lon + 1, bbInfo$max_lat - bbInfo$min_lat + 1, count),
    collapse = FALSE)
  close.nc(nc)
  if (any(is.na(invarValues))) {
    message("WARNING: NAs in invar values in year ", year, ", `fromIdx` ", fromIdx, ", `count` ", count)
  }
  stopifnot(length(dim(invarValues)) == 3)
  stopifnot(dim(invarValues)[3] == count)
  return(invarValues)
}

integrateDistribution <- function(distri, values) {
  stopifnot(identical(dim(distri), dim(values)[1:2]))
  stopifnot(length(dim(values)) == 3)
  integral <- apply(values, 3, \(x) sum(distri * x))
  return(integral)
}

getOutNcFilePath <- function(year) {
  file.path(
    .global$outDir,
    sprintf(.global$outNcFilePattern, year))
}

initOutNc <- function(years, regionNames, statisticNames) {
  for (year in years) {
    outNcFilePath <- getOutNcFilePath(year)
    if (file.exists(outNcFilePath)) {
      cat(outNcFilePath, " already exits. Do not recreate.\n")
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


saveResult <- function(results, year, regionName, statisticName, variableNames) {
  stopifnot(length(results) == length(variableNames))
  if (any(is.na(results))) {
    message(
      "Got NA results in year ", year,
      ", region ", regionName,
      ", statistic ", statisticName,
      ", variables ", paste(variableNames[is.na(results)], collapse=", "))
  }
  outNcFilePath <- getOutNcFilePath(year)
  outNc <- open.nc(outNcFilePath, write = TRUE, share = FALSE)
  regionNames <- var.get.nc(outNc, "region")
  regionIdx <- which(regionName == regionNames)
  stopifnot(length(regionIdx) == 1)
  statisticNames <- var.get.nc(outNc, "statistic")
  statisticIdx <- which(statisticName == statisticNames)
  stopifnot(length(statisticIdx) == 1)
  for (i in seq_along(variableNames)) {
    variableName <- variableNames[[i]]
    result <- results[[i]]
    if (!ncHasVariable(outNc, variableName)) {
      var.def.nc(outNc, variableName, "NC_DOUBLE", c("region", "statistic"), deflate = 9)
    }
    var.put.nc(
      outNc,
      variableName,
      result,
      start = c(regionIdx, statisticIdx),
      count = c(1, 1))
  }
  close.nc(outNc)
}

getYears <- function() {
  intersect(
    .global$popFileMeta$year,
    .global$invarFileMeta$year)
}
