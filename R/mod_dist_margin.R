#' dms UI Function
#'
#' @description A shiny Module to generate distribution margin
#'  inputs based on specs file.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param data Dataframe with specs.
#' @param n_sliders Vector of the same length as products for which distribution
#' margin input should be generated. 
#'
#' @noRd 
mod_dist_margin_ui <- function(id, data, n_sliders) {
  ns <- NS(id)
  tagList(
    dm_list <- vector("list", length(n_sliders)),
    for (i in 1:length(n_sliders)) {
      dm_list[[i]] <- numericInput(
        inputId = ns(snakecase::to_snake_case(data[i, 1])),
        label   = data[i, 1],
        min     = 0,
        max     = 1,
        step    = 0.01, 
        value   = data[i, 5]
      )
    },
    dm_list
  )
}
    
#' dms Server Function
#'
#' @noRd 
mod_dist_margin_server <- function(input, output, session,
                           data, validateButton, resetButton){
  ns <- session$ns
  
  const_dm <- data %>%
    dplyr::filter(.data$Min == .data$Max) %>%
    .$dm
  
  varying_dm <- data %>%
    dplyr::filter(.data$Min != .data$Max)
  
  assumptions <- reactiveVal()
  
  assumptions(list(dm = data$dm))
  
  observeEvent(validateButton(), {
    assumptions(list(
      dm = c(purrr::map_chr(
        snakecase::to_snake_case(varying_dm$Product), ~ input[[.x]]
      ),const_dm
    )))
  })
  
  observeEvent(resetButton(), {
    purrr::map2(
      .x = snakecase::to_snake_case(varying_dm$Product),
      .y = varying_dm$dm,
      ~updateNumericInput(session, inputId = .x, value   = .y)
    )
  })
  
  return(list(choice_dm = reactive({as.numeric(assumptions()$dm)})))
  
}
    
## To be copied in the UI
# mod_dms_ui("dms_ui_1", data, n_sliders)
    
## To be copied in the server
# callModule(mod_dms_server, "dms_ui_1")
 
