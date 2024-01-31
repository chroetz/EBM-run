# Default values of environment variables.
environmentVariables <- list(
  SNA_ROOT = "..",
  SNA_DATA = "{SNA_ROOT}/data",
  SNA_PROCESSED = "{SNA_ROOT}/processed",
  SNA_RUN = "{SNA_ROOT}/run",
  SNA_VARIABLE = "{SNA_ROOT}/variable",
  SNA_EXPLORE = "{SNA_ROOT}/explore"
)


# Check system environment variables and overwrite default values.
loadEnvironmentVariables <- function() {
  for (nm in names(environmentVariables)) {
    value <- Sys.getenv(nm)
    if (value != "") {
      environmentVariables[[nm]] <<- value
    }
  }
}


# Expand path by replacing environment variables and making path absolute.
expandPath <- function(path, mustWork = FALSE) {
  while(TRUE) {
    pathNew <- str_glue_data(environmentVariables, path)
    if (pathNew == path) {
      break
    }
    path <- pathNew
  }
  normalizePath(path, mustWork = mustWork)
}


expandAllVariablesEndingInPath <- function(lst) {
  loadEnvironmentVariables()
  for (i in seq_along(lst)) {
    if (is.character(lst[[i]]) && str_detect(names(lst)[i], "(Path$)|(Paths$)")) {
      for (j in seq_along(lst[[i]])) {
        lst[[i]][[j]] <- expandPath(lst[[i]][[j]])
      }
    } else if (is.list(lst[[i]])) {
      lst[[i]] <- expandAllVariablesEndingInPath(lst[[i]])
    }
  }
  return(lst)
}

