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
callScriptSlurm <- function(scriptFilePath, argList, prefix="EbmNetCdf", qos="standby", cpusPerTask=1) {
  stopifnot(isSlurmAvailable())
  for (args in argList) {
    jobName <- paste0(
      prefix, "_",
      format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), "_",
      gsub("[^a-zA-Z0-9]", "-", paste(args, collapse="-")))
    cat("Starting SLURM job", jobName, "\n")
    clcom <- paste0(
      "sbatch ",
      " --qos=", qos,
      " --nodes=1 --ntasks=1",
      " --cpus-per-task=", cpusPerTask,
      " --job-name=", jobName,
      " --output=", jobName, "_%j.out",
      " --error=", jobName, "_%j.err",
      " --mail-type=END",
      " --wrap=\"Rscript '", scriptFilePath, "' ",
      gsub("\"", "\\\\\"", paste(args, collapse=" ")), "\"")
    cat(clcom, "\n")
    system(clcom)
  }
}
