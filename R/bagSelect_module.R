
#' Bag Select Module
#' @rdname bagselect
#' @export
bagSelectUI <- function(id, woonplaats_multiple = FALSE){

  ns <- NS(id)

  tagList(
    selectInput(ns("sel_woonplaats"),
                label_tooltip("Woonplaats", "Selecteer eerst de woonplaats"),
                              choices = NULL, multiple = woonplaats_multiple),
    autocomplete_input(ns("sel_openbareruimtenaam"),
                       label_tooltip("Straat", paste("Typ de eerste letter van de straatnaam",
                                                     "om de opties te zien.",
                                                     "Zie je niks? Dan is er geen straat met",
                                                     "die naam in deze woonplaats")),
                       options = NULL, max_options = 100),
    selectizeInput(ns("sel_huisnummerhuisletter"), "Huisnummer", choices = NULL),
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

  updateSelectizeInput(session, "sel_woonplaats",
                       choices = sort(unique(bag$woonplaatsnaam)),
                       selected = "")

  observe({

    woonpl <- input$sel_woonplaats
    req(woonpl)

    bag_woonpl <- dplyr::filter(bag, woonplaatsnaam %in% !!woonpl)

    # Dit is vele malen sneller dan updateSelectInput
    # (zelfs met server = TRUE in updateSelectizeInput)
    update_autocomplete_input(session, "sel_openbareruimtenaam",
                              options = c("", sort(unique(bag_woonpl$openbareruimtenaam))),
                              value = "")

  })


  # Huisnummer/huisletter combo opties weergeven.
  observe({

    straat <- input$sel_openbareruimtenaam
    req(straat)

    bag_straat <- dplyr::filter(bag, openbareruimtenaam == !!straat)

    updateSelectizeInput(session, "sel_huisnummerhuisletter",
                         choices = c("", sort_leading_num(bag_straat$huisnummerhuisletter)),
                         selected = character(0))

  })


  # Reset filter
  observeEvent(input$btn_reset_bag, ignoreInit = TRUE, {

    updateSelectizeInput(session, "sel_woonplaats",
                         selected = character(0))

    updateSelectizeInput(session, "sel_huisnummerhuisletter",
                              selected = character(0))

    update_autocomplete_input(session, "sel_openbareruimtenaam",
                              value = "")

  })


  bag_selection <- reactive({

    straat <- input$sel_openbareruimtenaam
    hhl <- input$sel_huisnummerhuisletter

    if(is_empty(straat) | is_empty(hhl)){
      return(NULL)
    }

    out <- dplyr::filter(bag,
                  openbareruimtenaam == !!straat,
                  huisnummerhuisletter == !!hhl)

    if(nrow(out) == 0){
      return(NULL)
    } else {
      return(out)
    }

  })


  return(bag_selection)
}

