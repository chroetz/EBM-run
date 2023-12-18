.infoMask <- new.env()

#' @export
setupMaskSummation <- function(
  degStep,
  countryMaskPath,
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

  cat("\tGet regions ... ")
  regionNames <- getRegionNames(.infoMask)
  cat("\t", length(regionNames), "regions to process.\n")

  maskScalingValues <- NULL
  for (regionName in regionNames) {
    maskValues <- getMaskValues(.infoMask, regionName, onlyBoundingBox = FALSE)
    if (is.null(maskScalingValues)) {
      maskScalingValues <- maskValues
    } else {
      maskScalingValues <- maskScalingValues + maskValues
    }
  }

  cat("\tgetMaskScaling duration:", (proc.time()-pt)[3], "s\n")

  saveRDS(maskScalingValues, .infoMask$outFilePath)
}
