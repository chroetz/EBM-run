#' @export
interact <- function() {
  cat("Welcome to the R package run!\n")
  choice <- getUserInput(
    "Choose what to do:",
    c("slurm" = "use slurm to run a command with opts file",
      "run" = "directly run a command with opts file",
      "template" = "create a template opts file",
      "exit" = "exit the program"))
  switch(
    choice,
    run = interactRun(ignoreSlurm = TRUE),
    slurm = interactRun(ignoreSlurm = FALSE),
    template = interactTemplate(),
    exit = interactExit()
  )
}


interactExit <- function() {
  cat("Bye!\n")
}


interactRun <- function(ignoreSlurm) {
  choice <- getUserInput(
    "Choose what to do",
    c("search" = "search current directory for opts files",
      "enter" = "enter file path to opts file",
      "exit" = "exit the program"))
  switch(
    choice,
    search = interactRunSearch(ignoreSlurm),
    enter = interactRunEnter(ignoreSlurm),
    exit = interactExit()
  )
}


interactRunEnter <- function(ignoreSlurm) {
  while(TRUE) {
    cat(
      "Will run opts file ",
      if (ignoreSlurm) "directly" else "via slurm",
      ".\n",
      sep = "")
    cat("Enter file path to opts file (or nothing to exit):\n")
    optsFilePath <- getLine()
    if (nchar(optsFilePath) == 0) {
      cat("No file path entered. Exiting.\n")
      interactExit()
      return(invisible())
    }
    if (!file.exists(optsFilePath)) {
      cat("File does not exist. Try again.\n")
      next
    }
    break
  }
  runOptsFile(optsFilePath, ignoreSlurm)
}


interactRunSearch <- function(ignoreSlurm) {
  optsFilePaths <- list.files(
    pattern = ".json$",
    recursive = TRUE)
  if (length(optsFilePaths) == 0) {
    cat("No opts files found. Exiting.\n")
    interactExit()
    return(invisible())
  }
  choice <- getUserInput(
    paste0(
      "Choose opts file to run",
      if (ignoreSlurm) " (directly)" else " (via slurm)",
      ":"),
    optsFilePaths)
  choice <- normalizePath(choice, mustWork = TRUE)
  runOptsFile(choice, ignoreSlurm)
}


interactTemplate <- function() {
  runClasses <- getAvailableRunClasses()
  runClasses <- runClasses[sapply(runClasses, \(x) length(x) > 1)]
  runClasses <- lapply(runClasses, \(x) x[-length(x)])
  runClassStrings <- sapply(runClasses, paste0, collapse = ", ")
  chosenClassString <- getUserInput(
    "Which run class should the template be created for?",
    runClassStrings)
  chosenClass <- runClasses[[which(chosenClassString == runClassStrings)]]
  cat("Enter output file name:\n")
  outFilePath <- getLine()
  if (!endsWith(outFilePath, ".json")) {
    outFilePath <- paste0(outFilePath, ".json")
  }
  templateRunOpts(chosenClass, outFilePath)
}
