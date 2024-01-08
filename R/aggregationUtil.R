.infoInvar <- new.env()


processRegionYear <- function(regionName, year, invarNames, aggregationDistri, batchSize) {
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
    for (statisticName in .infoInvar$statisticNames) {
      cat("\t\t\tStatistic:", statisticName, "...")
      x <- calculateStatisticOnGrid(statisticName, invarValues)
      results <- integrateDistribution(aggregationDistri, x)
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
    max(abs(.infoInvar$grid$latValues - latValues)) < .infoInvar$eps,
    max(abs(.infoInvar$grid$lonValues - lonValues)) < .infoInvar$eps)
}

calculateStatisticOnGrid <- function(statisticName, invarValues) {
  .infoInvar$statisticFunctions[[statisticName]](invarValues)
}

getRegionNames <- function(info) {
  nc <- open.nc(info$countryMaskPath)
  varNames <- ncGetNonDimVariableNames(nc)
  close.nc(nc)
  return(varNames)
}


subsetRegion <- function(info, valuesOnTotalGrid, regionName) {
  nf <- info$idxBoundingBoxes |> dplyr::filter(GID_1 == regionName) |> as.list()
  len <- info$grid$latValues |> length()
  reversedLat <- reverseIndex(nf$min_lat, nf$max_lat, len)
  return(valuesOnTotalGrid[nf$min_lon:nf$max_lon, reversedLat$from:reversedLat$to, drop=FALSE])
}

getInvarFilePath <- function(year) {
  fileInfo <- .infoInvar$invarFileMeta |> filter(.data$year == .env$year)
  stopifnot(nrow(fileInfo) == 1)
  return(fileInfo$path)
}

getInvarNames <- function(year) {
  filePath <- getInvarFilePath(year)
  nc <- open.nc(filePath)
  invarNames <- var.get.nc(nc, .infoInvar$invarDimensionName)
  close.nc(nc)
  return(invarNames)
}

reverseArrayDim <- function(x, i) {
  DescTools::Rev(x, i)
}

reverseIndex <- function(from, to, len) {
  list(
    from = len - to + 1,
    to = len - from + 1,
    count = abs(from - to) + 1)
}

getMaskValues <- function(info, regionName, maskList, onlyBoundingBox = TRUE) {
  pt <- proc.time()
  if (onlyBoundingBox) {
    bbInfo <- info$idxBoundingBoxes |> dplyr::filter(GID_1 == regionName) |> as.list()
    values <- var.get.nc(
      maskList$nc,
      regionName,
      start = c(bbInfo$min_lon, bbInfo$min_lat),
      count = c(bbInfo$max_lon - bbInfo$min_lon + 1, bbInfo$max_lat - bbInfo$min_lat + 1),
      collapse = FALSE)
  } else {
    values <- var.get.nc(maskList$nc, regionName)
  }
  cat("\t\tvar.get.nc regi:", (proc.time()-pt)[3], "s\n")

  values <- reverseArrayDim(values, maskList$latIdx)
  cat("\t\treverseArrayDim:", (proc.time()-pt)[3], "s\n")

  if (any(is.na(values))) {
    message("WARNING: NAs in mask values in region ", regionName)
  }

  return(values)
}



checkInvar <- function(year, invarNames) {
  filePath <- getInvarFilePath(year)
  invar <- list()
  nc <- open.nc(filePath)
  invar$lonValues <- var.get.nc(nc, "lon")
  invar$latValues <- var.get.nc(nc, "lat")
  invar$dimensionValues <- var.get.nc(nc, .infoInvar$invarDimensionName)
  close.nc(nc)
  assertLonLat(invar$lonValues, invar$latValues)
  stopifnot(all(invarNames %in% invar$dimensionValues))
  return(invisible())
}

getInvarValues <- function(year, fromIdx, count, regionName) {
  bbInfo <- .infoInvar$idxBoundingBoxes |> dplyr::filter(GID_1 == regionName) |> as.list()
  filePath <- getInvarFilePath(year)
  # lat is reversed in the mask, i.e., also in the bounding box
  nc <- open.nc(filePath)
  latCount <- bbInfo$max_lat - bbInfo$min_lat + 1
  lonCount <- bbInfo$max_lon - bbInfo$min_lon + 1
  len <- .infoInvar$grid$latValues |> length()
  reversedLat <- reverseIndex(bbInfo$min_lat, bbInfo$max_lat, len)
  invarValues <- var.get.nc(
    nc,
    .infoInvar$invarValueVariableName,
    start = c(bbInfo$min_lon, reversedLat$from, fromIdx)[.infoInvar$lonLatVarToDimOrder],
    count = c(lonCount, latCount, count)[.infoInvar$lonLatVarToDimOrder],
    collapse = FALSE)
  close.nc(nc)
  if (any(is.na(invarValues))) {
    message("WARNING: NAs in invar values in year ", year, ", `fromIdx` ", fromIdx, ", `count` ", count)
  }
  if (!all(.infoInvar$lonLatVarToDimOrder == 1:3)) {
    invarValues <- aperm(invarValues, order(.infoInvar$lonLatVarToDimOrder))
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
    .infoInvar$outDir,
    sprintf(.infoInvar$outNcFilePattern, year))
}

initOutNc <- function(years, regionNames, statisticNames) {
  for (year in years) {
    outNcFilePath <- getOutNcFilePath(year)
    if (file.exists(outNcFilePath)) {
      cat(outNcFilePath, " already exits. Do not recreate.\n")
      next
    }
    outNc <- create.nc(outNcFilePath, format = "netcdf4", share = FALSE)
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
    if (!is.character(variableName)) {
      variableName <- paste0(.infoInvar$invarValueVariableName, "_", variableName)
    }
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

