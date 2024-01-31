
runOptsFileSlurm <- function(optsFilePath, startAfterJobIds = NULL, dependencyMode = NULL) {
  opts <- ConfigOpts::readOpts(optsFilePath)
  opts <- expandAllVariablesEndingInPath(opts)
  jobIds <- numeric(opts$slurm$nJobs)
  for (jobIdx in seq_len(opts$slurm$nJobs)) {
    prefix <- paste0(
      opts$slurm$prefix,
      "_",
      removeFileNameEnding(basename(optsFilePath)),
      "_",
      jobIdx)
    cmdExpression <- rlang::expr(cer::runOptsFile(!!optsFilePath, !!jobIdx))
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
  opts <- expandAllVariablesEndingInPath(opts)
  runOpts(opts, jobIdx)
  return(invisible())
}


runDependentJobsSlurm <- function(optsFilePath, jobIds) {
  opts <- ConfigOpts::readOpts(optsFilePath)
  opts <- expandAllVariablesEndingInPath(opts)
  if (hasValueString(opts$optsAfterwardsAlwaysFilePaths)) {
    for (optsFilePath in opts$optsAfterwardsAlwaysFilePaths) {
      runOptsFileSlurm(optsFilePath, startAfterJobIds = jobIds, dependencyMode = "afterany")
    }
  }
  if (hasValueString(opts$optsAfterwardsIfOkFilePaths)) {
    for (optsFilePath in opts$optsAfterwardsIfOkFilePaths) {
      runOptsFileSlurm(optsFilePath, startAfterJobIds = jobIds, dependencyMode = "afterok")
    }
  }
}


runDependentJobsDirect <- function(optsFilePath) {
  opts <- ConfigOpts::readOpts(optsFilePath)
  opts <- expandAllVariablesEndingInPath(opts)
  optsFilePathsAfterwards <-  c(
    opts$optsAfterwardsAlwaysFilePaths,
    opts$optsAfterwardsIfOkFilePaths)
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
  loadNamespace("cerProcessNetCdf") # to make sure its info is printed
  loadNamespace("cerExploreData") # to make sure its info is printed
  loadNamespace("cerUtility") # to make sure its info is printed
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
    Summary = runMethodSummary(opts),
    stop("Unknown method: ", subclass)
  )
  cat("\nEND Run", subclass, "after", (proc.time()-pt)[3], "s\n")
  cat("Run ended at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", sep="")
}
