#' Quickly View of PDF Help
#'
#' Una \ifelse{latex}{\out{funci{\'o}n}}{función} por su ayuda. View the PDF
#' help of a package or function.
#'
#' @param topic A name, the topic for which to produce a PDF. Must not be a
#'   character string (see examples).
#'
#' @return NULL. Called for its side-effect, which is to create a PDF at a
#'   temporary location and open it.
#' @export
#'
#' @note Requires the texi2dvi utility. It can be found in the *texinfo* package
#'   on debian and derivatives.
#' @source
#' [This](https://stackoverflow.com/questions/30607496/making-an-r-package-pdf-manual-using-devtools)
#' stackoverflow answer.
#' @examples
#'
#' \dontrun{
#'
#' ayuda(utils::help)
#' ayuda(utils)
#'
#' ayuda(charms::ayuda)
#' }
#'
ayuda <- function(topic) {

  topic <- as.character(substitute(topic))

  pkg <- if (topic[1] %in% c("::", ":::")) topic[2] else topic
  fun <- if (topic[1] %in% c("::", ":::")) topic[3] else NULL

  wd_old <- getwd()
  setwd("/tmp/")

  if (is.null(fun)) {
    path <- find.package(pkg)
    system(paste(shQuote(file.path(R.home("bin"), "R")),
               "CMD", "Rd2pdf", shQuote(path)))
  } else {
    h <- help(fun, package = eval(pkg), help_type = "pdf")
    print(h)
    system(paste0("xdg-open ",  fun, ".pdf"))
  }

  setwd(wd_old)
  invisible()
}
