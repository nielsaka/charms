#' Install, update and attach packages
#'
#' A utility function for easy installation and attaching of packages. If a
#' package is unavailable in any of the local libraries, it will be fetched and
#' installed.
#'
#' @param packages A character vector. The names of the packages to attach or
#'   install.
#' @param update A logical scalar. Should outdated packages be updated as well?
#' @param silent A logical scalar. Suppress all errors, warnings, and messages?
#'
#' @return A logical vector of the same length as `packages`. Each entry
#'   indicates whether loading the package was successful.
#' @export
#'
#' @examples
#' accio(c("tictoc", "praise", "available", "xxyyzz"))
accio <- function(packages, update = FALSE, silent = TRUE) {
  # TODO-9 non-standard evaluation for passing packages unquoted?

  if (silent) silencio()

  # installed.packages() includes all known trees
  # check .libPaths() to see which ones are known.
  pkgs2inst <- setdiff(packages, installed.packages())

  # add outdated packages to new packages

  # I'm thinking this assumes that someone really maintains this list... Maybe
  # its worth to double check that once in a while for important packages and if
  # not write a function that compares version numbers or so
  if (update) pkgs2inst <- c(pkgs2inst, packages[packages %in% old.packages()])

  if (length(pkgs2inst)) install.packages(pkgs2inst)
  l <- function() sapply(packages, require, character.only=T, quietly = silent)
  if (silent) {sonorus(); suppressWarnings(l())} else l()
}
