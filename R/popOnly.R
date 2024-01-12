#' @export
setupPopSummation <- function(
  degStep,
  maskPath,
  maskScalingPath,
  boundingBoxPath,
  popDir,
  popFileNamePattern,
  outFilePathPattern
) {
  argNames <- rlang::fn_fmls_names()
  env <- rlang::current_env()
  lapply(
    argNames,
    \(nm) assign(nm, env[[nm]], .info)
  )

  .info$eps <- sqrt(.Machine$double.eps)

  .info$grid <- list(
    lonValues = seq(-180, 180, by = degStep)[-1] - degStep/2,
    latValues = seq(-90, 90, by = degStep)[-1] - degStep/2)

  .info$idxBoundingBoxes <- readr::read_csv(boundingBoxPath, col_types = readr::cols())

  popFileNames <- list.files(popDir, pattern=popFileNamePattern)
  fileYears <- stringr::str_match(popFileNames, popFileNamePattern)[,2] |> as.integer()
  .info$popFileMeta <- tibble(
    year = fileYears,
    name = popFileNames,
    path = file.path(popDir, popFileNames))

  return(invisible())
}

getMaskScaling <- function(info) {
  maskScalingValues <- readRDS(info$maskScalingPath)
  return(maskScalingValues)
}


#' @export
runPopSummation <- function(yearsFilter = NULL) {

  cat("Get Mask Scaling:")
  maskScaling <- getMaskScaling(.info)
  cat("\n")

  cat("Get years ... ")
  years <- .info$popFileMeta$year
  if (!is.null(yearsFilter)) years <- intersect(years, yearsFilter)
  cat(length(years), "years to process.\n")

  cat("Get regions ... ")
  regionNames <- getRegionNames(.info)
  cat(length(regionNames), "regions to process.\n")

  cat("Open and check mask NC-File ... ")
  maskList <- openAndCheckMaskNc(.info$maskPath)
  cat("Done.\n")

  cat("Start main loop.\n")
  for (year in years) {
    cat("Year:", year, "\n")
    scaledPopValuesAll <- getPopValues(year, .info$popFileMeta) / maskScaling
    values <- vapply(
      regionNames,
      \(regionName) {
        pt <- proc.time()
        cat("\tRegion:", regionName, "\n")
        scaledPopValuesRegion <- subsetRegion(scaledPopValuesAll, regionName, .info$idxBoundingBoxes, .info$grid)
        maskValues <- getMaskValues(regionName, maskList, .info$idxBoundingBoxes)
        value <- sum(maskValues * scaledPopValuesRegion, na.rm = TRUE)
        cat("\tprocessRegionYear duration:", (proc.time()-pt)[3], "s\n")
        return(value)
      },
      double(1))
    result <- tibble(
      year = year,
      region = regionNames,
      population = values)
    readr::write_csv(result, sprintf(.info$outFilePathPattern, year))
  }
  cat("End main loop.\n")

  cat("Close mask NC-File ... ")
  close.nc(maskList$nc)
  cat("Done.\n")
}
