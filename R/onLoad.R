#' Add JS paths to shiny
#'
#' @importFrom shiny addResourcePath
#'
#' @noRd
.onLoad <- function(...) {
  shiny::addResourcePath('shintobag', system.file("assets", package = "shintobag"))
}

