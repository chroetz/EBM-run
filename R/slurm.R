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
callScriptSlurm <- function(
    scriptFilePath,
    argList,
    prefix = "EbmNetCdf",
    qos = c("standby", "priority", "io", "short", "medium", "long", "gpushort", "gpumedium", "gpulong", "gpupreempt"),
    #partition = c("priority", "io", "gpu", "largemem", "standard"), # Need to match qos to partitions...
    cpusPerTask = 1,
    timeInMinutes = NULL,
    mail = TRUE,
    logDir = "_log"
) {
  qos <- match.arg(qos)
  #partition <- match.arg(partition)
  stopifnot(isSlurmAvailable())
  if (!dir.exists(logDir)) dir.create(logDir)
  for (args in argList) {
    jobName <- paste0(
      prefix, "_",
      format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), "_",
      gsub("[^a-zA-Z0-9]", "-", paste(args, collapse="-")))
    cat("Starting SLURM job", jobName, "\n")
    clcom <- paste0(
      "sbatch ",
      " --qos=", qos,
      #" --partition=", partition,
      " --nodes=1 --ntasks=1",
      " --cpus-per-task=", cpusPerTask,
      " --job-name=", jobName,
      " --output=", file.path(logDir, paste0(jobName, "_%j.out")),
      " --error=", file.path(logDir, paste0(jobName, "_%j.err")),
      if (!is.null(timeInMinutes)) paste0(" --time ", timeInMinutes),
      if (mail) " --mail-type=END",
      " --wrap=\"Rscript '", scriptFilePath, "' ",
      gsub("\"", "\\\\\"", paste(args, collapse=" ")), "\"")
    cat(clcom, "\n")
    system(clcom)
  }
}
