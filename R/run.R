#' @export
run <- function(optsFilePath) {

  opts <- jsonlite::read_json(optsFilePath)

  pt <- proc.time()
  cat("START Run", opts$method, "\n\n")
  switch(
    opts$method,
    ShapeToMask = runMethodShapeToMask(opts),
    BoundingBoxes = runMethodBoundingBoxes(opts),
    SumMask = runMethodSumMask(opts),
    SumAggregation = runMethodSumAggregation(opts),
    stop("Unknown method: ", opts$method)
  )
  cat("\nEND Run", opts$method, "after", (proc.time()-pt)[3], "s\n")
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

  setupSumAggregation(
    targetFormat = opts$targetFormat,
    maskFilePath = opts$maskFilePath,
    maskSumFilePath = opts$maskSumFilePath,
    boundingBoxFilePath = opts$boundingBoxFilePath,
    variableDataDescriptor = opts$variableDataDescriptor,
    outFilePath = opts$outFilePath
  )

  runSumAggregation(opts$yearsFilter, opts$regionIndices)
}


runMethodShapeToMask <- function(opts) {

  if (opts$oneFilePerRegion) {

    filePaths <- list.files(opts$shapeFileDir, full.names = TRUE, recursive=TRUE)
    shapeFilePaths <- stringr::str_subset(filePaths, opts$shapeFilePattern)
    names(shapeFilePaths) <- uniqueMiddle(shapeFilePaths)

    cat("Found", length(shapeFilePaths), "shape files.\n")

    runShapeToMaskOneFilePerRegion(
      shapeFilePaths = shapeFilePaths,
      nLon = opts$nLon,
      nLat = opts$nLat,
      outFilePath = opts$outFilePath
    )

  } else {

    runShapeToMaskOneFileForAllRegions(
      shapeFilePath = opts$shapeFilePath,
      nLon = opts$nLon,
      nLat = opts$nLat,
      outFilePrefix = opts$outFilePrefix,
      metaOutFilePath = opts$metaOutFilePath,
      idColumnName = opts$idColumnName,
      batchSize = opts$batchSize,
      batchIndexFilter = opts$batchIndexFilter
    )
  }
}
