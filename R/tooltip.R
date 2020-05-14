#' Handy tooltip for Shiny
#'@export
label_tooltip <- function(label, helptext){
  
  HTML(glue::glue("{label} <span class='glyphicon glyphicon-info-sign' ",
                  "data-toggle = 'tooltip' ",
                  "data-original-title='Help' ", 
                  "title='{helptext}' ",
                  "></span>"))
}
