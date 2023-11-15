#' @export
startComp <- function(cmdStr, prefix="EbmNetCdf") {
  if (isSlurmAvailable()) {
    jobName <- paste0(prefix, "_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
    cat("Starting SLURM job", jobName, "\n")
    clcom <- paste0(
      "sbatch ",
      " --qos=short",
      " --job-name=", jobName,
      " --output=", jobName, "_%j.out",
      " --error=", jobName, "_%j.err",
      " --mail-type=END",
      " --wrap=\"Rscript -e '", gsub("\"", "\\\\\"", cmdStr), "'\"")
    cat(clcom, "\n")
    system(clcom)
  } else {
    stop("Slurm is not available.")
  }
}

isSlurmAvailable <- function() {
  return(suppressWarnings(system2("srun", stdout = FALSE, stderr = FALSE) != 127))
}


#' @export
callScriptSlurm <- function(scriptFilePath, argList, prefix="EbmNetCdf") {
  stopifnot(isSlurmAvailable())
  for (i in nrow(argTibble)) {
    args <- argList[[i]]
    jobName <- paste0(prefix, "_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
    cat("Starting SLURM job", jobName, "\n")
    clcom <- paste0(
      "sbatch ",
      " --qos=short",
      " --job-name=", jobName,
      " --output=", jobName, "_%j.out",
      " --error=", jobName, "_%j.err",
      " --mail-type=END",
      " --wrap=\"Rscript '", scriptFilePath, "' ",
      gsub("\"", "\\\\\"", paste(args, collapse=" ")), "'\"")
    cat(clcom, "\n")
    system(clcom)
  }
}
