extractArgs <- function(opts, ..., .remove = NULL) {
  specifiedArgs <- list(...)
  runNames <- ConfigOpts::getDefaultOpts("Run") |> names()
  optsNames <- names(opts)
  args <- unclass(opts)
  args <- args[setdiff(optsNames, c(runNames, .remove))]
  for (nm in names(specifiedArgs)) {
    args[[nm]] <- specifiedArgs[[nm]]
  }
  return(args)
}
