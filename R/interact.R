#' @export
. <- structure(NA, class = "InteractionStarter")

#' @export
print.InteractionStarter <- function(x, ...) {
  interact()
}

#' @export
interact <- function() {
  cat("Hello World!\n")
  # TODO
}
