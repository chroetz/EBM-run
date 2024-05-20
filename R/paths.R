# Default values of environment variables.
environmentVariables <- new.env(parent = emptyenv())
environmentVariablesDefault <- list(
  CER_ROOT = "..",
  CER_DATA = "{CER_ROOT}/data",
  CER_EXPLORE = "{CER_ROOT}/explore",
  CER_PROCESSED = "{CER_ROOT}/processed",
  CER_REGRESSION = "{CER_ROOT}/regression",
  CER_RUN = "{CER_ROOT}/run",
  CER_VARIABLE = "{CER_ROOT}/variable")


# Check system environment variables and overwrite default values.
loadEnvironmentVariables <- function() {
  lapply(
    names(environmentVariablesDefault),
    \(nm) assign(nm, environmentVariablesDefault[[nm]], envir = environmentVariables))
  snaNames <- Sys.getenv(names = TRUE) |> names() |> str_subset("^CER_")
  for (nm in snaNames) {
    environmentVariables[[nm]] <- Sys.getenv(nm)
  }
}


# Expand path by replacing environment variables and making path absolute.
expandPath <- function(path, mustWork = FALSE) {
  while(TRUE) {
    pathNew <- str_glue_data(environmentVariables, path)
    if (pathNew == path) break
    path <- pathNew
  }
  normalizePath(path, mustWork = mustWork)
}


expandAllVariablesEndingInPath <- function(lst) {
  loadEnvironmentVariables()
  if (length(names(lst)) == 0) {
    if (is.list(lst)) {
      for (i in seq_along(lst)) {
        lst[[i]] <- expandAllVariablesEndingInPath(lst[[i]])
      }
    }
    return(lst)
  }
  for (i in seq_along(lst)) {
    hasPathName <- str_detect(names(lst)[i], "(Path$)|(Paths$)")
    if (is.character(lst[[i]]) && hasPathName) {
      for (j in seq_along(lst[[i]])) {
        lst[[i]][[j]] <- expandPath(lst[[i]][[j]])
      }
    } else if (is.list(lst[[i]]) && hasPathName) {
      for (j in seq_along(lst[[i]])) {
        if (is.character(lst[[i]][[j]])) {
          lst[[i]][[j]] <- expandPath(lst[[i]][[j]])
        }
      }
    } else if (is.list(lst[[i]])) {
      lst[[i]] <- expandAllVariablesEndingInPath(lst[[i]])
    }
  }
  return(lst)
}

