#' @export
setupMaskSummation <- function(
  maskFilePath,
  outFilePath
) {

  # put function arguments into environment .info
  argNames <- rlang::fn_fmls_names()
  env <- rlang::current_env()
  lapply(
    argNames,
    \(nm) assign(nm, env[[nm]], .info))

  gridFormat <- getNativeGridFormat(maskFilePath)
  initializeGrid(gridFormat)

  return(invisible())
}


#' @export
runMaskSummation <- function() {

  pt <- proc.time()

  cat("Get regions ... ")
  regionNames <- getRegionNames(.info$maskPath)
  cat(length(regionNames), "regions to process.\n")

  cat("Open and check mask NC-File ... ")
  maskList <- openAndCheckMaskNc(.info$maskPath)
  cat("Done.\n")

  maskScalingValues <- NULL
  for (regionName in regionNames) {
    pt <- proc.time()
    cat("Process region '", regionName, "' ... ", sep="")
    maskValues <- getMaskValues(regionName, maskList)
    if (is.null(maskScalingValues)) {
      maskScalingValues <- maskValues
    } else {
      maskScalingValues <- maskScalingValues + maskValues
    }
    cat("Done in ", (proc.time()-pt)[3], "s.\n")
  }

  cat("Close mask NC-File ... ")
  close.nc(maskList$nc)
  cat("Done.\n")

  cat("getMaskScaling duration:", (proc.time()-pt)[3], "s\n")

  cat("Save mask scaling values ... ")
  saveNetCdf(
    .info$outFilePath,
    list(lon = .info$grid$lonValues, lat = .info$grid$latValues),
    maskScalingValues)
  cat("Done.\n")
}
