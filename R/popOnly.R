.infoPop <- new.env()

#' @export
setupPopSummation <- function(
  degStep,
  countryMaskPath,
  boundingBoxPath,
  popDir,
  popFileNamePattern,
  outFilePathPattern
) {
  argNames <- rlang::fn_fmls_names()
  env <- rlang::current_env()
  lapply(
    argNames,
    \(nm) assign(nm, env[[nm]], .infoPop)
  )

  .infoPop$eps <- sqrt(.Machine$double.eps)

  .infoPop$grid <- list(
    lonValues = seq(-180, 180, by = degStep)[-1] - degStep/2,
    latValues = seq(-90, 90, by = degStep)[-1] - degStep/2)

  .infoPop$idxBoundingBoxes <- readr::read_csv(boundingBoxPath, col_types = readr::cols())

  popFileNames <- list.files(popDir, pattern=popFileNamePattern)
  fileYears <- stringr::str_match(popFileNames, popFileNamePattern)[,2] |> as.integer()
  .infoPop$popFileMeta <- tibble(
    year = fileYears,
    name = popFileNames,
    path = file.path(popDir, popFileNames))

  return(invisible())
}

getMaskScaling <- function() {
  # TODO: calculate this only once and save it.

  pt <- proc.time()

  cat("\tGet regions ... ")
  regionNames <- getRegionNames(.infoPop)
  cat("\t", length(regionNames), "regions to process.\n")

  maskScalingValues <- NULL
  for (regionName in regionNames) {
    maskValues <- getMaskValues(.infoPop, regionName, onlyBoundingBox = FALSE)
    if (is.null(maskScalingValues)) {
      maskScalingValues <- maskValues
    } else {
      maskScalingValues <- maskScalingValues + maskValues
    }
  }

  cat("\tgetMaskScaling duration:", (proc.time()-pt)[3], "s\n")

  return(maskScalingValues)
}


#' @export
runPopSummation <- function(yearsFilter = NULL) {

  cat("Get Mask Scaling:")
  maskScaling <- getMaskScaling()
  cat("\n")

  cat("Get years ... ")
  years <- .infoPop$popFileMeta$year
  if (!is.null(yearsFilter)) years <- intersect(years, yearsFilter)
  cat(length(years), "years to process.\n")

  cat("Get regions ... ")
  regionNames <- getRegionNames(.infoPop)
  cat(length(regionNames), "regions to process.\n")

  cat("Start main loop.\n")
  for (year in years) {
    cat("Year:", year, "\n")
    scaledPopValuesAll <- getPopValues(.infoPop, year) / maskScaling
    values <- vapply(
      regionNames,
      \(regionName) {
        pt <- proc.time()
        cat("\tRegion:", regionName, "\n")
        scaledPopValuesRegion <- subsetRegion(.infoPop, scaledPopValuesAll, regionName)
        maskValues <- getMaskValues(.infoPop, regionName)
        value <- sum(maskValues * scaledPopValuesRegion)
        cat("\tprocessRegionYear duration:", (proc.time()-pt)[3], "s\n")
        return(value)
      },
      double(1))
    result <- tibble::tibble(
      year = year,
      region = regionNames,
      population = values)
    readr::write_csv(result, sprintf(.infoPop$outFilePathPattern, year))
  }
  cat("End main loop.\n")
}
