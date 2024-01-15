#' @export
runOptsFile <- function(optsFilePath, ignoreSlurm = FALSE, jobIdx = NULL) {
  opts <- ConfigOpts::readOpts(optsFilePath, optsClass = "Run")
  if (ignoreSlurm) {
    runOpts(opts, jobIdx)
    return(invisible())
  }
  for (jobIdx in seq_len(opts$slurm$nJobs)) {
    cmdExpression <- rlang::expr(run::runOptsFile(!!optsFilePath, TRUE, !!jobIdx))
    executeCodeViaSlurm(
      cmdStr = rlang::expr_text(cmdExpression, width = 500),
      prefix = opts$slurm$prefix,
      qos = opts$slurm$qos,
      cpusPerTask = opts$slurm$cpusPerTask,
      timeInMinutes = opts$slurm$timeInMinutes,
      mail = opts$slurm$mail,
      logDir = opts$slurm$logDir)
  }
}


#' @export
runOpts <- function(opts, jobIdx=NULL) {

  opts <- ConfigOpts::asOpts(opts, c("Run"))
  if (hasValue(jobIdx)) {
    opts <- ConfigOpts::overwriteOpts(opts, list(slurm = list(jobIdx = jobIdx)))
  }

  cat("Run started at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n", sep="")
  loadNamespace("ProcessNetCdf") # to make sure its info is printed
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
  boundingBoxes <- ProcessNetCdf::getBoundingBoxesFromMask(maskFilePath)
  cat("obtained", ncol(boundingBoxes), "bounding boxes in ", (proc.time()-pt)[3],"s\n")
  cat("saving bounding boxes to file ", outFilePath, "... ")
  ProcessNetCdf::saveBoundingBoxes(
    boundingBoxes,
    outFilePath,
    maskFilePath,
    regionVariableName = "regionName")
  cat("done.\n")
}


runMethodSumMask <- function(opts) {

  opts <- ConfigOpts::asOpts(opts, c("SumMask", "Run"))

  ProcessNetCdf::setupMaskSummation(
    maskFilePath = opts$maskFilePath,
    outFilePath = opts$outFilePath
  )

  ProcessNetCdf::runMaskSummation()
}


runMethodSumAggregation <- function(opts) {

  opts <- ConfigOpts::asOpts(opts, c("SumAggregation", "Run"))

  ProcessNetCdf::setupSumAggregation(
    targetFormat = opts$targetFormat,
    maskFilePath = opts$maskFilePath,
    maskSumFilePath = opts$maskSumFilePath,
    boundingBoxFilePath = opts$boundingBoxFilePath,
    variableDataDescriptor = opts$variableDataDescriptor,
    outFilePath = opts$outFilePath
  )

  ProcessNetCdf::runSumAggregation(opts$yearsFilter, opts$regionIndices)
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

  ProcessNetCdf::runShapeToMaskOneFilePerRegion(
    shapeFilePaths = shapeFilePaths,
    nLon = opts$nLon,
    nLat = opts$nLat,
    outFilePath = opts$outFilePath
  )
}


runMethodShapeToMaskOneFileForAllRegions <- function(opts) {

  opts <- ConfigOpts::asOpts(opts, c("OneFileForAllRegions", "ShapeToMask", "Run"))

  ProcessNetCdf::runShapeToMaskOneFileForAllRegions(
    shapeFilePath = opts$shapeFilePath,
    nLon = opts$nLon,
    nLat = opts$nLat,
    outFilePrefix = opts$outFilePrefix,
    metaOutFilePath = opts$metaOutFilePath,
    idColumnName = opts$idColumnName,
    nBatches = opts$slurm$nJobs,
    batchIndexFilter = opts$slurm$jobIdx
  )
}
