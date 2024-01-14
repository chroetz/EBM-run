#' @export
templateRunOpts <- function(runOptsClass, outFilePath = NULL) {
  optsClass <- c(runOptsClass, "Run")
  opts <- ConfigOpts::getDefaultOpts(
    optsClass,
    removeUnderscoreEntries = FALSE,
    fill = TRUE)
  if (hasValue(outFilePath)) {
    ConfigOpts::writeOpts(opts, outFilePath, addMetaInfo = FALSE)
  } else {
    cat(format(opts), "\n")
  }
  return(invisible())
}


#' @export
printAvailableRunClasses <- function() {
  cat("Available run classes:\n")
  runClasses <- getAvailableRunClasses()
  lapply(runClasses, dput)
  return(invisible())
}


getAvailableRunClasses <- function() {
  runClasses <- ConfigOpts::getAvailableOptsClasses()
  runClasses <- runClasses[sapply(runClasses, \(x) last(x) == "Run")]
  return(runClasses)
}
