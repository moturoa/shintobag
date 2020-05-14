#' Attach dependencies
#'
#' @noRd
#' @importFrom utils packageVersion
#' @importFrom htmltools htmlDependency attachDependencies
attachDependencies <- function(tag){

  version <- as.character(packageVersion("shintobag")[[1]])

  dep <- list(
    htmltools::htmlDependency(
      "tooltip", version,
      src = c(href = "shintobag/tooltip"),
      script = "tooltip.js",
      stylesheet = "tooltip.css"
    )
  )

htmltools::attachDependencies(tag, dep, append = TRUE)
}


