#' @export
run <- function(optsFilePath) {

  opts <- ConfigOpts::readOpts(optsFilePath, optsClass = "Run")

  cat("Run started at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n", sep="")
  printPackagesInfo()
  cat("Run Opts:\n", format(opts), "\n\n", sep="")

  subclass <- ConfigOpts::getClassAt(opts, 2)

  pt <- proc.time()
  cat("START Run", subclass, "\n\n")
  switch(
    subclass,
    ShapeToMask = runMethodShapeToMask(opts),
    BoundingBoxes = runMethodBoundingBoxes(opts),
    SumMask = runMethodSumMask(opts),
    SumAggregation = runMethodSumAggregation(opts),
    stop("Unknown method: ", subclass)
  )
  cat("\nEND Run", subclass, "after", (proc.time()-pt)[3], "s\n")
  cat("Run ended at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", sep="")
}


runMethodBoundingBoxes <- function(opts) {

  opts <- ConfigOpts::asOpts(opts, c("BoundingBoxes", "Run"))

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
}


runMethodSumMask <- function(opts) {

  opts <- ConfigOpts::asOpts(opts, c("SumMask", "Run"))

  setupMaskSummation(
    maskFilePath = opts$maskFilePath,
    outFilePath = opts$outFilePath
  )

  runMaskSummation()
}


runMethodSumAggregation <- function(opts) {

  opts <- ConfigOpts::asOpts(opts, c("SumAggregation", "Run"))

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

  opts <- ConfigOpts::asOpts(opts, c("ShapeToMask", "Run"))

  subclass <- ConfigOpts::getClassAt(opts, 3)

  switch(
    subclass,
    OneFilePerRegion = runMethodShapeToMaskOneFilePerRegion(opts),
    OneFileForAllRegions = runMethodShapeToMaskOneFileForAllRegions(opts),
    stop("Unknown subclass: ", subclass)
  )
}


runMethodShapeToMaskOneFilePerRegion <- function(opts) {

  opts <- ConfigOpts::asOpts(opts, c("OneFilePerRegion", "ShapeToMask", "Run"))

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
}


runMethodShapeToMaskOneFileForAllRegions <- function(opts) {

  opts <- ConfigOpts::asOpts(opts, c("OneFileForAllRegions", "ShapeToMask", "Run"))

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
