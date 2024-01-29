#' @export
. <- structure(NA, class = "InteractionStarter")

#' @export
s <- structure(NA, class = c("SlurmInteractionStarter"))

#' @export
ss <- structure(NA, class = c("SearchSlurmInteractionStarter"))

#' @export
print.InteractionStarter <- function(x, ...) {
  interact()
}

#' @export
print.SlurmInteractionStarter <- function(x, ...) {
  interactRun(useSlurm = TRUE)
}

#' @export
print.SearchSlurmInteractionStarter <- function(x, ...) {
  interactRunSearch(useSlurm = TRUE)
}
