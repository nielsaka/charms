###############################################################################.
#' Silence All Output
#'
#' Silence all output including messages, warnings and errors that would
#' otherwise be printed to the console or terminal. Use your `sonorus()` charm
#' to disable.
#'
#' If you dislike Harry Potter, you would have to play a round of `hide()` and
#' `seek()` instead. However, these are not exported to avoid confusion with
#' [base::seek()].
#'
#' @return NULL
#' @export
#'
#' @examples
#' warning("You!")
#'
#' #---------
#' silencio()
#' #---------
#'
#' warning("You!")
#' stop("You, again!")
#'
#' #--------
#' sonorus()
#' #--------
#'
#' message("finally, it stopped")
#'
#' charms:::hide()
#' print("this works, too?")
#' charms:::seek()
#' cat("yes")
silencio <- function() {
  nul <- if (Sys.info()["sysname"] == "Windows") "NUL" else "/dev/null"
  zz <- file(nul, open = "wt")
  sink(file = zz, type = "message")
  sink(zz, type = "output")
}
###############################################################################.
#' @export
#'
#' @rdname silencio
sonorus <- function() {
  sink(type = "message")
  sink(type = "output")
}
###############################################################################.
hide <- silencio
seek <- sonorus