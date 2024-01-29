
runOptsFileSlurm <- function(optsFilePath, startAfterJobIds = NULL, dependencyMode = NULL) {
  opts <- ConfigOpts::readOpts(optsFilePath)
  jobIds <- numeric(opts$slurm$nJobs)
  for (jobIdx in seq_len(opts$slurm$nJobs)) {
    prefix <- paste0(
      opts$slurm$prefix,
      "_",
      removeFileNameEnding(basename(optsFilePath)),
      "_",
      jobIdx)
    cmdExpression <- rlang::expr(run::runOptsFile(!!optsFilePath, !!jobIdx))
    jobIds[jobIdx] <- executeCodeViaSlurm(
      cmdStr = rlang::expr_text(cmdExpression, width = 500),
      prefix = prefix,
      qos = opts$slurm$qos,
      cpusPerTask = opts$slurm$cpusPerTask,
      timeInMinutes = opts$slurm$timeInMinutes,
      mail = opts$slurm$mail,
      logDir = opts$slurm$logDir,
      startAfterJobIds = startAfterJobIds,
      dependencyMode = dependencyMode)
  }
  runDependentJobsSlurm(optsFilePath, jobIds)
  return(jobIds)
}


runOptsFileDirect <- function(optsFilePath) {
  runOptsFile(optsFilePath)
  runDependentJobsDirect(optsFilePath)
}


#' @export
runOptsFile <- function(optsFilePath, jobIdx = NULL) {
  opts <- ConfigOpts::readOpts(optsFilePath)
  runOpts(opts, jobIdx)
  return(invisible())
}


runDependentJobsSlurm <- function(optsFilePath, jobIds) {
  opts <- ConfigOpts::readOpts(optsFilePath)
  if (hasValueString(opts$optsFilePathsAfterwardsAlways)) {
    for (optsFilePath in opts$optsFilePathsAfterwardsAlways) {
      runOptsFileSlurm(optsFilePath, startAfterJobIds = jobIds, dependencyMode = "afterany")
    }
  }
  if (hasValueString(opts$optsFilePathsAfterwardsIfOk)) {
    for (optsFilePath in opts$optsFilePathsAfterwardsIfOk) {
      runOptsFileSlurm(optsFilePath, startAfterJobIds = jobIds, dependencyMode = "afterok")
    }
  }
}

runDependentJobsDirect <- function(optsFilePath) {
  opts <- ConfigOpts::readOpts(optsFilePath)
  optsFilePathsAfterwards <-  c(
    opts$optsFilePathsAfterwardsAlways,
    opts$optsFilePathsAfterwardsIfok)
  if (hasValueString(optsFilePathsAfterwards)) {
    for (optsFilePathAfterwards in optsFilePathsAfterwards) {
      runOptsFileDirect(optsFilePathAfterwards)
    }
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
    AggregateNaryMasked = runMethodAggregateNaryMasked(opts),
    ConcatNetCdf = runMethodConcatNetCdf(opts),
    CreateMaps = runMethodCreateMaps(opts),
    ImagesToVideo = runMethodImagesToVideo(opts),
    stop("Unknown method: ", subclass)
  )
  cat("\nEND Run", subclass, "after", (proc.time()-pt)[3], "s\n")
  cat("Run ended at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", sep="")
}
