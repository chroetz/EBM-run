#' @export
runOptsFile <- function(optsFilePath, ignoreSlurm = FALSE, jobIdx = NULL) {
  opts <- ConfigOpts::readOpts(optsFilePath)
  if (ignoreSlurm) {
    runOpts(opts, jobIdx)
    return(invisible())
  }
  for (jobIdx in seq_len(opts$slurm$nJobs)) {
    prefix <- paste0(
      opts$slurm$prefix,
      "_",
      removeFileNameEnding(basename(optsFilePath)),
      "_",
      jobIdx)
    cmdExpression <- rlang::expr(run::runOptsFile(!!optsFilePath, TRUE, !!jobIdx))
    executeCodeViaSlurm(
      cmdStr = rlang::expr_text(cmdExpression, width = 500),
      prefix = prefix,
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
    ConcatNetCdf = runMethodConcatNetCdf(opts),
    CreateMaps = runMethodCreateMaps(opts),
    ImagesToVideo = runMethodImagesToVideo(opts),
    stop("Unknown method: ", subclass)
  )
  cat("\nEND Run", subclass, "after", (proc.time()-pt)[3], "s\n")
  cat("Run ended at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", sep="")
}
