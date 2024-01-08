#' @export
setupPopWeightAggregation <- function(
  degStep,
  countryMaskPath,
  boundingBoxPath,
  popDir = NULL,
  popFileNamePattern = NULL,
  invarDir,
  invarFileNamePattern,
  invarDimensionName,
  invarValueVariableName,
  lonLatVarToDimOrder = 1:3,
  outDir,
  outNcFilePattern,
  batchSize
) {
  argNames <- rlang::fn_fmls_names()
  env <- rlang::current_env()
  lapply(
    argNames,
    \(nm) assign(nm, env[[nm]], .infoInvar)
  )

  .infoInvar$eps <- sqrt(.Machine$double.eps)

  .infoInvar$grid <- list(
    lonValues = seq(-180, 180, by = degStep)[-1] - degStep/2,
    latValues = seq(-90, 90, by = degStep)[-1] - degStep/2)

  .infoInvar$idxBoundingBoxes <- readr::read_csv(boundingBoxPath, col_types = readr::cols())

  if (!is.null(popDir) && !is.null(popFileNamePattern)) {
    popFileNames <- list.files(popDir, pattern=popFileNamePattern)
    fileYears <- stringr::str_match(popFileNames, popFileNamePattern)[,2] |> as.integer()
    .infoInvar$popFileMeta <- tibble(
      year = fileYears,
      name = popFileNames,
      path = file.path(popDir, popFileNames))
    .infoInvar$weightByPop <- TRUE
  } else {
    .infoInvar$weightByPop <- FALSE
  }

  invarFileNames <- list.files(
    invarDir,
    pattern = invarFileNamePattern)
  fileYears <- stringr::str_match(invarFileNames, invarFileNamePattern)[,2] |> as.integer()
  .infoInvar$invarFileMeta <- tibble(
    year = fileYears,
    name = invarFileNames,
    path = file.path(invarDir, invarFileNames))
  return(invisible())
}


#' @export
setupPopWeightAggregationStatistics <- function(...) {
  args <- list(...)
  .infoInvar$statisticNames <- names(args)
  .infoInvar$statisticFunctions <- args
  return(invisible())
}


#' @export
runPopWeightAggregation <- function(yearsFilter = NULL, invarNamesIdxFilter = NULL) {
  cat("Get years ... ")
  years <- getYearsPop()
  if (!is.null(yearsFilter)) years <- intersect(years, yearsFilter)
  cat(length(years), "years to process.\n")

  cat("Get regions ... ")
  regionNames <- getRegionNames(.infoInvar)
  cat(length(regionNames), "regions to process.\n")

  cat("Open and check mask NC-File ... ")
  maskList <- list()
  maskList$nc <- open.nc(.infoInvar$countryMaskPath)
  maskList$lonValues <- var.get.nc(maskList$nc, "lon")
  maskList$latValues <- var.get.nc(maskList$nc, "lat")
  assertLonLat(maskList$lonValues, rev(maskList$latValues))
  maskList$latIdx <- ncGetDimensionIndex(maskList$nc, "lat")
  cat("Done.\n")

  cat("Initializing output files...\n")
  initOutNc(years, regionNames, .infoInvar$statisticNames)
  cat("Done.\n")

  cat("Start main loop.\n")
  for (year in years) {
    cat("Year:", year, "\n")

    outNcFilePath <- getOutNcFilePath(year)
    outNc <- open.nc(outNcFilePath, write = TRUE, share = FALSE)

    if (.infoInvar$weightByPop) {
      popValuesAll <- getPopValues(.infoInvar, year)
    }
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
    fullyFilledRegionNames <- getFullyFilledRegionNames(.infoInvar, year, invarNames, outNc=outNc)
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
      pt <- proc.time()
      maskValues <- getMaskValues(.infoInvar, regionName, maskList = maskList)
      if (.infoInvar$weightByPop) {
        popValuesRegion <- subsetRegion(.infoInvar, popValuesAll, regionName)
        aggregationDistri <- calcPopRegionDistri(popValuesRegion, maskValues)
      } else {
        aggregationDistri <- maskValues
      }
      cat("\tload mask duration:", (proc.time()-pt)[3], "s\n")
      pt <- proc.time()
      processRegionYear(
        regionName,
        year,
        invarNames,
        aggregationDistri,
        batchSize = .infoInvar$batchSize,
        outNc = outNc)
      cat("\tprocessRegionYear duration:", (proc.time()-pt)[3], "s\n")

      # TODO:
      close.nc(outNc)
      outNc <- open.nc(outNcFilePath, write = TRUE, share = FALSE)
    }

    close.nc(outNc)
  }
  cat("End main loop.\n")

  cat("Close mask NC-File ... ")
  close.nc(maskList$nc)
  cat("Done.\n")
}


getPopValues <- function(info, year) {
  fileInfo <-
    info$popFileMeta |>
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


calcPopRegionDistri <- function(popValues, maskValues) {
  stopifnot(identical(dim(maskValues), dim(popValues)))
  maskPop <- maskValues * popValues
  maskPopDistri <- maskPop / pmax(1, sum(maskPop))
  return(maskPopDistri)
}



getYearsPop <- function() {
  if (.infoInvar$weightByPop) {
    return(
      intersect(
        .infoInvar$popFileMeta$year,
        .infoInvar$invarFileMeta$year))
  } else {
    return(.infoInvar$invarFileMeta$year)
  }
}
