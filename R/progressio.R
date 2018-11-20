

#' Title
#'
#' @param iterations
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#' progrede <- progressio(100)
#' replicate(100, progrede())
#'
#'
#' progrede <- progressio(100)
#' replicate(100, {Sys.sleep(0.05); progrede()})
#'
#'
#' TODO: option to output progress on command line
progressio <- function(iterations, title = "Progress") {

  label <- "0 % completed; estimated time: -- "

  conf <- list(
    title = title,
    label = label,
    min = 0,
    max = 100,
    initial = 0,
    width = 500)

  # is_windows <- Sys.info()["sysname"] == "Windows"

  # if (is_windows) {
    # pb <- do.call(utils::winProgressBar, conf)
  # } else {
    # works on windows too!
    pb <- do.call(tcltk::tkProgressBar, conf)
  # }

  delayedAssign("earlier", Sys.time())

  cntr <- 0
  function() {
    cntr <<- cntr + 1

    progress <- cntr / iterations * 100
    running <- difftime(Sys.time(), earlier, units = "mins")
    remaining <- round(running * (1 / progress * 100 - 1), 1)


    if (progress %% 1 == 0) {
      label <<- paste(
        progress, " % completed;
        time running:", round(running, 1), "min;",
        "remaining:", remaining, "min")
    }
    tcltk::setTkProgressBar(pb, value = progress, label = label)
  }
}


# wrap a function with progress bar

#' Title
#'
#' @param fun
#' @param iterations
#'
#' @return
#' @export
#'
#' @examples
#'
#' shout <- function() Sys.sleep(0.1)
#'
#' reps <- 100
#' shout <- prgr(shout, reps)
#'
#' Sys.sleep(3)
#' replicate(reps, shout())
#'
prgr <- function(fun, iterations) {
  force(fun)
  title <- deparse(substitute(fun))
  progrede <- progressio(iterations, title)

  function(...) {
    progrede()
    fun(...)
  }
}

