library(shiny)
library(shintobag)
library(dplyr)

bag <- readRDS("../cache/bag_Eindhoven_sf.rds")



ui <- fluidPage(

  actionButton("btn_go","Test"),
  tags$hr(),
  uiOutput("ui_out")

)

server <- function(input, output, session) {

  rv <- reactiveValues(
    data = NULL
  )

  observeEvent(input$btn_go,{

    rv$data <- sample_n(bag, 1)

  })

  output$ui_out <- renderUI({

    req(rv$data)

    externe_bronnen_lokatie(rv$data)

  })


}

shinyApp(ui, server)
