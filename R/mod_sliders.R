#' sliders UI Function
#'
#' @description A shiny Module to generate price sliders based on specs file.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param data Dataframe with specs.
#' @param n_sliders Vector of the same length as products for which cost sliders 
#' should be generated.
#' @noRd 
#'
mod_sliders_ui <- function(id, data, n_sliders){
  ns <- NS(id)
  tagList(
    sliders_list <- vector("list", length(n_sliders)),
    for (i in 1:length(n_sliders)) {
      sliders_list[[i]] <- sliderInput(
        inputId = ns(snakecase::to_snake_case(data[i, 1])),
        label   = data[i, 1],
        min     = data[i, 2],
        max     = data[i, 4],
        value   = data[i, 3]
      )
    },
    sliders_list
  )
}

#' sliders Server Function
#'
#' @noRd 
mod_sliders_server <- function(input, output, session,
                               data, validateButton, resetButton){
  ns <- session$ns
  
  const_prices <- data %>%
    dplyr::filter(.data$Min == .data$Max) %>%
    .$Base
  
  varying_prices <- data %>%
    filter(.data$Min != .data$Max)
  
  assumptions <- reactiveVal()
  
  assumptions(list(price = data$Base))
  
  observeEvent(validateButton(), {
    assumptions(list(price = c(
      purrr::map_chr(
        snakecase::to_snake_case(varying_prices$Product), ~ input[[.x]]
      ),
      const_prices
    )))
  })
  
  observeEvent(resetButton(), {
    purrr::map2(
      .x = snakecase::to_snake_case(varying_prices$Product),
      .y = varying_prices$Base,
      ~updateSliderInput(session, inputId = .x, value   = .y)
    )
  })
  
  return(list(choice_price = reactive({as.numeric(assumptions()$price)})))
 
}
    
## To be copied in the UI
# mod_sliders_ui("sliders_ui_1")
    
## To be copied in the server
# callModule(mod_sliders_server, "sliders_ui_1", reactive(input$validate))
 
