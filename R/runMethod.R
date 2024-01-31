

runMethodBoundingBoxes <- function(opts) {

  opts <- ConfigOpts::asOpts(opts, c("BoundingBoxes", "Run"))

  maskFilePath <- opts$maskFilePath

  outFilePath <- paste0(
    stringr::str_sub(maskFilePath, end=-4),
    "_boundingBox.nc")

  pt <- proc.time()
  boundingBoxes <- cerProcessNetCdf::getBoundingBoxesFromMask(maskFilePath)
  cat("obtained", ncol(boundingBoxes), "bounding boxes in ", (proc.time()-pt)[3],"s\n")
  cat("saving bounding boxes to file ", outFilePath, "... ")
  cerProcessNetCdf::saveBoundingBoxes(
    boundingBoxes,
    outFilePath,
    maskFilePath,
    regionVariableName = opts$regionVariableName)
  cat("done.\n")
}


runMethodSumMask <- function(opts) {

  opts <- ConfigOpts::asOpts(opts, c("SumMask", "Run"))

  args <- extractArgs(opts)

  do.call(cerProcessNetCdf::setupMaskSummation, args)
  cerProcessNetCdf::runMaskSummation()
}


runMethodAggregateNaryMasked <- function(opts) {

  opts <- ConfigOpts::asOpts(opts, c("AggregateNaryMasked", "Run"))

  exprList <- lapply(opts$aggregateTextList, rlang::parse_expr)
  names(exprList) <- names(opts$aggregateTextList)

  args <- extractArgs(
    opts,
    nBatches = opts$slurm$nJobs,
    batchIndex = opts$slurm$jobIdx,
    outFilePath = paste0(opts$outFilePrefix, "_", opts$slurm$jobIdx, ".csv"),
    aggregateExpression = exprList,
    .remove = c("aggregateTextList", "outFilePrefix"))

  do.call(cerProcessNetCdf::aggregateNaryMasked, args)
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
  names(shapeFilePaths) <- cerUtility::uniqueMiddle(shapeFilePaths)

  cat("Found", length(shapeFilePaths), "shape files.\n")

  args <- extractArgs(
    opts,
    shapeFilePaths = shapeFilePaths,
    .remove = c("shapeFileDirPath", "shapeFilePattern"))

  do.call(cerProcessNetCdf::runShapeToMaskOneFilePerRegion, args, quote = TRUE)
}


runMethodShapeToMaskOneFileForAllRegions <- function(opts) {

  opts <- ConfigOpts::asOpts(opts, c("OneFileForAllRegions", "ShapeToMask", "Run"))

  if (opts$slurm$jobIdx > 1) {
    metaOutFilePath <- NULL
  } else {
    metaOutFilePath <- opts$metaOutFilePath
  }

  args <- extractArgs(
    opts,
    outFilePath = paste0(opts$outFilePrefix, "_", opts$slurm$jobIdx, ".nc"),
    metaOutFilePath = metaOutFilePath,
    nBatches = opts$slurm$nJobs,
    batchIndex = opts$slurm$jobIdx,
    .remove = c("metaOutFilePath", "outFilePrefix"))

  do.call(cerProcessNetCdf::runShapeToMaskOneFileForAllRegions, args, quote = TRUE)
}


runMethodConcatNetCdf <- function(opts) {

  opts <- ConfigOpts::asOpts(opts, c("ConcatNetCdf", "Run"))

  args <- extractArgs(opts)

  do.call(cerProcessNetCdf::runConcatNetCdf, args, quote = TRUE)
}


runMethodCreateMaps <- function(opts) {

  opts <- ConfigOpts::asOpts(opts, c("CreateMaps", "Run"))

  args <- extractArgs(
    opts,
    nBatches = opts$slurm$nJobs,
    batchIndex = opts$slurm$jobIdx)

  do.call(cerExploreData::createMaps, args, quote = TRUE)
}


runMethodImagesToVideo <- function(opts) {

  opts <- ConfigOpts::asOpts(opts, c("ImagesToVideo", "Run"))

  args <- extractArgs(
    opts,
    nBatches = opts$slurm$nJobs,
    batchIndex = opts$slurm$jobIdx)

  do.call(cerExploreData::createVideo, args, quote = TRUE)
}


runMethodSummary <- function(opts) {

  opts <- ConfigOpts::asOpts(opts, c("Summary", "Run"))

  args <- extractArgs(
    opts,
    outDir = dirname(opts$outFilePath),
    outFileName = cerUtility::removeFileNameEnding(basename(opts$outFilePath)),
    outFormat = cerUtility::getFileNameEnding(opts$outFilePath),
    aggregateFunctions =
      opts$aggregateFunctionsText |>
      rlang::parse_expr(),
    transformations =
      opts$transformationsText |>
      rlang::parse_expr(),
    .remove = c(
      "outFilePath",
      "aggregateFunctionsText",
      "transformationsText"))

  do.call(cerExploreData::renderSummary, args, quote = TRUE)
}
