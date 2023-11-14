#' @export
getRootDir <- function() {
  rootDir <-
    if (dir.exists("//clusterfs.pik-potsdam.de/projects")) {
      "//clusterfs.pik-potsdam.de"
    } else if (dir.exists("/p/projects")) {
      "/p"
    } else {
      stop("Cannot find root dir.")
    }
  return(rootDir)
}
