

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

  do.call(cerProcessNetCdf::sumMask, args)
}


runMethodAggregateNaryMasked <- function(opts) {

  opts <- ConfigOpts::asOpts(opts, c("AggregateNaryMasked", "Run"))

  exprList <- lapply(opts$aggregateTextList, rlang::parse_expr)
  names(exprList) <- names(opts$aggregateTextList)

  args <- extractArgs(
    opts,
    nBatches = opts$slurm$nJobs,
    batchIndex = opts$slurm$jobIdx,
    outFilePath = paste0(opts$outFilePrefixPath, "_", opts$slurm$jobIdx, ".csv"),
    aggregateExpression = exprList,
    .remove = c("aggregateTextList", "outFilePrefixPath"))

  do.call(cerProcessNetCdf::aggregateNaryMasked, args)
}


runMethodRollTimeApply <- function(opts) {

  opts <- ConfigOpts::asOpts(opts, c("RollTimeApply", "Run"))

  args <- extractArgs(
    opts,
    nBatches = opts$slurm$nJobs,
    batchIndex = opts$slurm$jobIdx)

  do.call(cerProcessNetCdf::rollTimeApply, args)
}


runMethodRollTimeApplyConcat <- function(opts) {

  opts <- ConfigOpts::asOpts(opts, c("RollTimeApplyConcat", "Run"))

  args <- extractArgs(
    opts,
    nBatches = opts$slurm$nJobs,
    batchIndex = opts$slurm$jobIdx)

  do.call(cerProcessNetCdf::concatAfterRoll, args)
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
    outFilePath = paste0(opts$outFilePrefixPath, "_", opts$slurm$jobIdx, ".nc"),
    metaOutFilePath = metaOutFilePath,
    nBatches = opts$slurm$nJobs,
    batchIndex = opts$slurm$jobIdx,
    .remove = c("metaOutFilePath", "outFilePrefixPath"))

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
    nBatches = opts$slurm$nJobs,
    batchIndex = opts$slurm$jobIdx,
    aggregateFunctions =
      opts$aggregateFunctionsText |>
      rlang::parse_expr(),
    transformations =
      opts$transformationsText |>
      rlang::parse_expr(),
    .remove = c(
      "aggregateFunctionsText",
      "transformationsText"))

  do.call(cerExploreData::renderSummary, args, quote = TRUE)
}


runMethodRegression <- function(opts) {

  opts <- ConfigOpts::asOpts(opts, c("Regression", "Run"))

  variableTable <- lapply(
    opts$variableListList,
    \(lt) {
      lt |>
        lapply(\(x) if (length(x) == 1) x else list(x)) |>
        as_tibble()
    }) |>
    bind_rows()

  args <- extractArgs(
    opts,
    nBatches = opts$slurm$nJobs,
    batchIndex = opts$slurm$jobIdx,
    variableTable = variableTable,
    .remove = c("variableListList"))

  do.call(cerStatistic::runRegressions, args, quote = TRUE)
}
