#' @export
run <- function(optsFilePath) {
  opts <- jsonlite::read_json(optsFilePath)
  switch(
    opts$method,
    BoundingBoxes = runMethodBoundingBoxes(opts),
    SumMask = runMethodSumMask(opts),
    SumAggregation = runMethodSumAggregation(opts),
    stop("Unknown method: ", opts$method)
  )
}


runMethodBoundingBoxes <- function(opts) {

  maskFilePath <- opts$maskFilePath

  outFilePath <- paste0(
    stringr::str_sub(maskFilePath, end=-4),
    "_boundingBox.nc")

  pt <- proc.time()
  boundingBoxes <- getBoundingBoxesFromMask(maskFilePath)
  cat("obtained", ncol(boundingBoxes), "bounding boxes in ", (proc.time()-pt)[3],"s\n")
  cat("saving bounding boxes to file ", outFilePath, "... ")
  saveBoundingBoxes(
    boundingBoxes,
    outFilePath,
    maskFilePath,
    regionVariableName = "regionName")
  cat("done.\n")
  cat("\n")
}


runMethodSumMask <- function(opts) {

  setupMaskSummation(
    maskFilePath = opts$maskFilePath,
    outFilePath = opts$outFilePath
  )

  runMaskSummation()

}


runMethodSumAggregation <- function(opts) {

  setupPopSummation(
    degStep = arcmin/60,
    countryMaskPath = file.path(
      rootDir,
      paste0(baseName, ".nc")),
    maskScalingPath = file.path(
      rootDir,
      paste0(baseName, "_sum.RDS")),
    boundingBoxPath = file.path(
      rootDir,
      paste0(baseName, "_boundingBox.csv")),
    popDir = file.path(
      rootDir,
      "projects/isimip/.snapshots/daily.2023-12-11-21-00/isimip/tovogt/isimip3a_tc/output/pop_5arcmin"),
      #"projects/isimip/isimip/tovogt/isimip3a_tc/output/pop_5arcmin"),
    popFileNamePattern = "^popc_(\\d+).nc$",
    outFilePathPattern = paste0(sprintf("maskDoseRegionArcmin%d_fractional", arcmin), "_population_%d.csv")
  )

  runPopSummation(year)


}
