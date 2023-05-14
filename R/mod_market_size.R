#' universe UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'

mod_market_size_ui <- function(id){
  ns <- NS(id)
  tagList(
    numericInput(ns("ms"), label = "Market size", value = 100000)
  )
}
    
#' universe Server Function
#'
#' @noRd 
mod_universe_server <- function(input, output, session, validateButton){
  ns <- session$ns
 
  assumptions <- reactiveVal()
  
  assumptions(list(market_size = 100000))
  
  observeEvent(validateButton(), {
    assumptions(list(market_size = input$ms))
  })
  return(list(choice_ms = reactive({assumptions()$market_size})))
}
    
## To be copied in the UI
# mod_universe_ui("universe_ui_1")
    
## To be copied in the server
# callModule(mod_universe_server, "universe_ui_1")
 
