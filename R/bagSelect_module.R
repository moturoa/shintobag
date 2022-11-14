#' Bag Select Module
#' @description
#' `r lifecycle::badge('deprecated')`
#' @rdname bagselect
#' @export
bagSelectUI <- function(id,
                        woonplaats_multiple = FALSE,
                        reset_button = TRUE){

  ns <- NS(id)

  tagList(
    selectInput(ns("sel_woonplaats"),
                label_tooltip("Woonplaats", "Selecteer eerst de woonplaats"),
                choices = NULL, multiple = woonplaats_multiple),


    selectInput(ns("sel_openbareruimtenaam"),
                label_tooltip("Straat", paste("Typ de eerste letter van de straatnaam",
                                              "om de opties te zien.",
                                              "Zie je niks? Dan is er geen straat met",
                                              "die naam in deze woonplaats")),
                choices = NULL, multiple = FALSE),


    selectizeInput(ns("sel_huisnummerhuisletter"), "Huisnummer", choices = NULL),
    if(reset_button){
      tags$div(style = "padding-top: 25px; ",
               actionButton(ns("btn_reset_bag"), "Reset",
                            icon = icon("refresh"),
                            class = "btn-info btn-sm bag_reset_btn")
      )
    } else NULL

  )

}





#' @rdname bagselect
#' @export
bagSelect <- function(input, output, session, bag,
                      reset_button = reactive(NULL),
                      enkel_adres = TRUE,
                      allow_null_woonplaats = TRUE,
                      ui_straat = c("autocomplete","selectInput"),
                      ping = reactive(runif(1))){

  ui_straat <- match.arg(ui_straat)

  bag_straten <- sort(unique(bag$openbareruimtenaam))


  if(!all(c("huisnummerhuisletter","bag_adres") %in% names(bag))){
    stop("BAG moet bewerkt worden met add_bag_adres_kolommen()")
  }

  observeEvent(ping(), {
    updateSelectizeInput(session, "sel_woonplaats",
                         choices = sort(unique(bag$woonplaatsnaam)),
                         selected = "")

    if(allow_null_woonplaats){

        updateSelectizeInput(session, "sel_openbareruimtenaam",
                             choices = c("", bag_straten),
                             selected = "", server = TRUE)

    }
  })

  observeEvent(input$sel_woonplaats, {

    bag_woonpl <- dplyr::filter(bag, woonplaatsnaam %in% !!input$sel_woonplaats)


    updateSelectizeInput(session, "sel_openbareruimtenaam",
                         choices = c("", sort(unique(bag_woonpl$openbareruimtenaam))),
                         selected = "", server = TRUE)


  })


  # Huisnummer/huisletter combo opties weergeven.
  observeEvent(input$sel_openbareruimtenaam, {

    bag_straat <- dplyr::filter(bag, openbareruimtenaam == !!input$sel_openbareruimtenaam)

    updateSelectizeInput(session, "sel_huisnummerhuisletter",
                         choices = c("", sort_leading_num(bag_straat$huisnummerhuisletter)),
                         selected = character(0))

  })


  reset_filters <- function(){
    updateSelectizeInput(session, "sel_woonplaats",
                         selected = character(0))

    updateSelectizeInput(session, "sel_huisnummerhuisletter",
                         selected = character(0))


  }

  # Reset filter
  observeEvent(input$btn_reset_bag, ignoreInit = TRUE,  reset_filters())
  observeEvent(reset_button(), ignoreInit = TRUE,  reset_filters())


  bag_selection <- reactive({

    woonpl <- input$sel_woonplaats
    straat <- input$sel_openbareruimtenaam
    hhl <- input$sel_huisnummerhuisletter

    if(enkel_adres && (is_empty(straat) | is_empty(hhl))){
      return(NULL)
    }

    out <- bag %>%
      filter_in("openbareruimtenaam", straat) %>%
      filter_in("huisnummerhuisletter", hhl)

    if(!is.null(woonpl)){
      out <- filter_in(out, "woonplaatsnaam", woonpl)
    }


    if(nrow(out) == 0){
      return(NULL)
    } else {
      return(out)
    }

  })


  return(bag_selection)
}
