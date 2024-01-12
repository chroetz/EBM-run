.infoMask <- new.env()

#' @export
setupMaskSummation <- function(
  degStep,
  maskPath,
  outFilePath
) {
  argNames <- rlang::fn_fmls_names()
  env <- rlang::current_env()
  lapply(
    argNames,
    \(nm) assign(nm, env[[nm]], .infoMask)
  )

  .infoMask$eps <- sqrt(.Machine$double.eps)

  .infoMask$grid <- list(
    lonValues = seq(-180, 180, by = degStep)[-1] - degStep/2,
    latValues = seq(-90, 90, by = degStep)[-1] - degStep/2)

  return(invisible())
}


#' @export
runMaskSummation <- function() {

  pt <- proc.time()

  cat("Get regions ... ")
  regionNames <- getRegionNames(.infoInvar)
  cat(length(regionNames), "regions to process.\n")

  cat("Open and check mask NC-File ... ")
  maskList <- openAndCheckMaskNc(.infoInvar$maskPath)
  cat("Done.\n")

  maskScalingValues <- NULL
  for (regionName in regionNames) {
    maskValues <- getMaskValues(.infoMask, regionName, onlyBoundingBox = FALSE)
    if (is.null(maskScalingValues)) {
      maskScalingValues <- maskValues
    } else {
      maskScalingValues <- maskScalingValues + maskValues
    }
  }

  cat("Close mask NC-File ... ")
  close.nc(maskList$nc)
  cat("Done.\n")

  cat("getMaskScaling duration:", (proc.time()-pt)[3], "s\n")

  cat("Save mask scaling values ... ")
  saveNetCdf(
    .infoMask$outFilePath,
    list(lon = .infoMask$grid$lonValues, lat = .infoMask$grid$latValues),
    maskScalingValues)
  cat("Done.\n")
}
