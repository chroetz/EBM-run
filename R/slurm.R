#' @export
executeCodeViaSlurm <- function(
    cmdStr,
    prefix = "run",
    qos = c("standby", "priority", "io", "short", "medium", "long", "gpushort", "gpumedium", "gpulong", "gpupreempt"),
    cpusPerTask = 1,
    timeInMinutes = NULL,
    mail = TRUE,
    logDir = "_log"
) {
  if (isSlurmAvailable()) {
    jobName <- paste0(prefix, "_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
    cat("Starting SLURM job", jobName, "\n")
    escapedCmdStr <- gsub("\"", "\\\\\"", cmdStr)
    escapedCmdStr <- gsub("'", "\\\\\'", escapedCmdStr)
    if (!dir.exists(logDir)) dir.create(logDir)
    clcom <- paste0(
      "sbatch ",
      " --qos=", qos,
      " --nodes=1 --ntasks=1",
      " --cpus-per-task=", cpusPerTask,
      " --job-name=", jobName,
      " --output=", file.path(logDir, paste0(jobName, "_%j.out")),
      " --error=", file.path(logDir, paste0(jobName, "_%j.err")),
      if (hasValue(timeInMinutes)) paste0(" --time ", timeInMinutes),
      if (mail) " --mail-type=END",
      " --wrap=\"Rscript -e '", escapedCmdStr, "'\"")
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
executeScriptViaSlurm <- function(
    scriptFilePath,
    argList,
    prefix = "run",
    qos = c("standby", "priority", "io", "short", "medium", "long", "gpushort", "gpumedium", "gpulong", "gpupreempt"),
    cpusPerTask = 1,
    timeInMinutes = NULL,
    mail = TRUE,
    logDir = "_log"
) {
  qos <- match.arg(qos)
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
      " --nodes=1 --ntasks=1",
      " --cpus-per-task=", cpusPerTask,
      " --job-name=", jobName,
      " --output=", file.path(logDir, paste0(jobName, "_%j.out")),
      " --error=", file.path(logDir, paste0(jobName, "_%j.err")),
      if (hasValue(timeInMinutes)) paste0(" --time ", timeInMinutes),
      if (mail) " --mail-type=END",
      " --wrap=\"Rscript '", scriptFilePath, "'",
      " ", gsub("\"", "\\\\\"", paste(args, collapse=" ")), "\"")
    cat(clcom, "\n")
    system(clcom)
  }
}
