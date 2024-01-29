#' @export
executeCodeViaSlurm <- function(
    cmdStr,
    prefix = "run",
    qos = c("standby", "priority", "io", "short", "medium", "long", "gpushort", "gpumedium", "gpulong", "gpupreempt"),
    cpusPerTask = 1,
    timeInMinutes = NULL,
    mail = TRUE,
    logDir = "_log",
    startAfterJobIds = NULL,
    dependencyMode = NULL
) {
  if (isSlurmAvailable()) {
    jobName <- paste0(prefix, "_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
    cat("Starting SLURM job", jobName, "\n")
    escapedCmdStr <- gsub("\"", "\\\\\"", cmdStr)
    escapedCmdStr <- gsub("'", "\\\\\'", escapedCmdStr)
    if (!dir.exists(logDir)) dir.create(logDir)
    command <- createSlurmCommand(
      qos, cpusPerTask, jobName, logDir, timeInMinutes, mail, startAfterJobIds, dependencyMode,
      paste0("\"Rscript -e '", escapedCmdStr, "'\""))
    cat(command, "\n")
    output <- system(command, intern = TRUE)
    jobId <- extractJobId(output)
    return(jobId)
  } else {
    stop("Slurm is not available.")
  }
}


createSlurmCommand <- function(
    qos,
    cpusPerTask,
    jobName,
    logDir,
    timeInMinutes,
    mail,
    startAfterJobIds,
    dependencyMode,
    wrap
) {
  command <- paste0(
    "sbatch ",
    " --qos=", qos,
    " --nodes=1 --ntasks=1",
    " --cpus-per-task=", cpusPerTask,
    " --job-name=", jobName,
    " --output=", file.path(logDir, paste0(jobName, "_%j.out")),
    " --error=", file.path(logDir, paste0(jobName, "_%j.err")),
    if (hasValue(timeInMinutes)) paste0(" --time ", timeInMinutes),
    if (mail) " --mail-type=END",
    if (hasValue(startAfterJobIds)) paste0(" --dependency=", dependencyMode, ":", paste(startAfterJobIds, collapse=":")),
    " --wrap=", wrap)
  return(command)
}


extractJobId <- function(x) {
  if (startsWith(x, "Submitted batch job ")) {
    return(as.numeric(substring(x, 21)))
  } else {
    return(NA)
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
    logDir = "_log",
    startAfterJobIds = NULL
) {
  qos <- match.arg(qos)
  stopifnot(isSlurmAvailable())
  if (!dir.exists(logDir)) dir.create(logDir)
  for (i in seq_along(argList)) {
    args <- argList[[i]]
    jobName <- paste0(
      prefix, "_",
      format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), "_",
      gsub("[^a-zA-Z0-9]", "-", paste(args, collapse="-")))
    cat("Starting SLURM job", jobName, "\n")
    command <- createSlurmCommand(
      qos, cpusPerTask, jobName, logDir, timeInMinutes, mail, startAfterJobIds,
      paste0(
        "\"Rscript '", scriptFilePath, "'",
        " ", gsub("\"", "\\\\\"", paste(args, collapse=" ")), "\""))
    cat(command, "\n")
    output <- system(command, intern = TRUE)
    jobId <- extractJobId(output)
    names(argList)[i] <- as.character(jobId)
  }
  return(argList)
}
