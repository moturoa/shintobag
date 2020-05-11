
#' Bag Select Module
#' @rdname bagselect
#' @export
bagSelectUI <- function(id){

  ns <- NS(id)

  tagList(
    autocomplete_input(ns("sel_openbareruimtenaam"), "Straat", options = NULL, max_options = 100),
    selectInput(ns("sel_huisnummerhuisletter"), "Huisnummer", choices = NULL), #, max_options = 100),
    tags$div(style = "padding-top: 25px; ",
             actionButton(ns("btn_reset_bag"), "Reset",
                          icon = icon("refresh"),
                          class = "btn-info btn-sm bag_reset_btn")
    )
  )

}


#' @rdname bagselect
#' @export
bagSelect <- function(input, output, session, bag){


  if(!all(c("huisnummerhuisletter","bag_adres") %in% names(bag))){
    stop("BAG moet bewerkt worden met add_bag_adres_kolommen()")
  }

  # Zet achtergrond kleur van de autocomplete_select (default is lelijk geel)
  # handige truc (https://stackoverflow.com/questions/57367387/how-to-define-css-for-elements-inside-shiny-modules)
  # nu dat dit in een package staat kan het ook in een gebundelde CSS
  css_auto_option <- paste0("$('head').append('<style type=\"text/css\">",
                            ".autocomplete-items div:hover{background-color: #EDEDED;}",
                            "</style>');")
  shinyjs::runjs(css_auto_option)

  # Dit is vele malen sneller dan updateSelectInput
  # (zelfs met server = TRUE in updateSelectizeInput)
  update_autocomplete_input(session, "sel_openbareruimtenaam",
                            options = c("", sort(unique(bag$openbareruimtenaam))),
                            value = "")

  # Huisnummer/huisletter combo opties weergeven.
  observe({

    straat <- input$sel_openbareruimtenaam
    req(straat)

    bag_straat <- dplyr::filter(bag, openbareruimtenaam == !!straat)

    updateSelectizeInput(session, "sel_huisnummerhuisletter",
                         choices = c("", sort_leading_num(bag_straat$huisnummerhuisletter)),
                         selected = character(0), server = TRUE)

  })


  # Reset filter
  observeEvent(input$btn_reset_bag, ignoreInit = TRUE, {

    update_autocomplete_input(session, "sel_huisnummerhuisletter",
                              value = "")

    update_autocomplete_input(session, "sel_openbareruimtenaam",
                              value = "")


  })


  bag_selection <- reactive({

    req(input$sel_openbareruimtenaam)
    req(input$sel_huisnummerhuisletter)

    dplyr::filter(bag,
                  openbareruimtenaam == !!input$sel_openbareruimtenaam,
                  huisnummerhuisletter == !!input$sel_huisnummerhuisletter)

  })


  return(bag_selection)
}

