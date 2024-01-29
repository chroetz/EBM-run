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


interactRunSearch <- function(ignoreSlurm, path = ".") {
  optsFilePaths <- list.files(
    path = path,
    pattern = ".json$",
    recursive = FALSE)
  dirPaths <- c(
    "..",
    list.dirs(
      path = path,
      recursive = FALSE,
      full.names = FALSE)
  )
  optsFilePrefix <- "run "
  dirPrefix <- "goto "
  choices <- c(
    if (length(optsFilePaths) > 0)
      paste0(optsFilePrefix, optsFilePaths)
    else
      character(0),
    if (length(dirPrefix) > 0)
      paste0(dirPrefix, dirPaths)
    else
        character(0))
  choice <- getUserInput(
    paste0(
      "Choose opts file to run",
      if (ignoreSlurm) " (directly)" else " (via slurm)",
      ":"),
    choices)
  if (startsWith(choice, optsFilePrefix)) {
    optsFilePath <- normalizePath(
      file.path(path, substring(choice, nchar(optsFilePrefix)+1)),
      mustWork = TRUE)
    runOptsFile(optsFilePath, ignoreSlurm)
  } else if (startsWith(choice, dirPrefix)) {
    interactRunSearch(ignoreSlurm, normalizePath(
      file.path(path, substring(choice, nchar(dirPrefix)+1)),
      mustWork = TRUE))
  } else {
    stop("Unknown choice: ", choice)
  }
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
