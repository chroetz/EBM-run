#' @export
run <- function(optsFilePath) {
  opts <- jsonlite::read_json(optsFilePath)
  switch(
    opts$method,
    BoundingBoxes = runMethodBoundingBoxes(opts),
    SumMask = runMethodSumMask(opts),
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
    regionVariableName = "GID_1")
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
