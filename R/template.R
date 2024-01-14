#' @export
templateRunOpts <- function(optsClass, outFilePath = NULL) {
  opts <- ConfigOpts::getDefaultOpts(optsClass, removeUnderscoreEntries = FALSE)
  if (hasValue(outFilePath)) {
    ConfigOpts::writeOpts(opts, outFilePath, addMetaInfo = FALSE)
  } else {
    cat(format(opts), "\n")
  }
  return(invisible())
}


#' @export
availableRunClasses <- function() {
  cat("Available run classes:\n")
  lapply(
    ConfigOpts::getAvailableOptsClasses(),
    \(x) {
      if (!last(x) == "Run") return(NULL)
      dput(x)
    }
  )
  return(invisible())
}
