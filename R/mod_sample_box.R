#' sample_boxes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param data Data frame with utilities.
#' @param respid_key Vector with Respondents ID to filter.
#' @param type Character. "total" or "sample"
#'
#' @noRd 
mod_sample_boxes_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    br(),
    uiOutput(ns("sampleBox"))
  )
}
    
#' sample_boxes Server Function
#'
#' @noRd 
mod_sample_boxes_server <- function(input,
                                    output,
                                    session,
                                    data,
                                    respid_key,
                                    type){
  
  output$sampleBox <- renderUI({
    ns <- session$ns
    
    sample_size <- calc_sample(respid_key())
    tot_size <- nrow(data)
    
    if (type == "total") {
      valueBox(tot_size,
               "Total sample size",
               icon = icon("thermometer-full"),
               color = "blue",
               width = 12)
    } else if (type == "sample") {
      valueBox(sample_size,
               "Selected sample size",
               icon = icon("thermometer-half"),
               color = "blue",
               width = 12)
    }
  })
}
    
## To be copied in the UI
# mod_sample_boxes_ui("sample_boxes_ui_1")
    
## To be copied in the server
# callModule(mod_sample_boxes_server, "sample_boxes_ui_1", data, respid_key, type)
 
