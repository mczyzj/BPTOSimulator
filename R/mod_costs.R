#' costs UI Function
#'
#' @description A shiny Module to generate cost inputs based on specs file.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param data Dataframe with specs.
#' @param names_num_input Names vector of the same length as products for which 
#'   cost input fileds should be generated.
#' @param validateButton Object that triggers validation.
#' @param resetButton Object that triggers reset to default settings.
#'
#' @noRd
mod_costs_ui <- function(id, data, names_num_input) {
  ns <- NS(id)
  tagList(
    dm_list <- vector("list", length(names_num_input)),
    for (i in 1:length(names_num_input)) {
      dm_list[[i]] <- numericInput(
        inputId = ns(snakecase::to_snake_case(data[i, 1])),
        label   = data[i, 1],
        min     = 0,
        step    = 0.1, 
        value   = data[i, 6]
        )
      },
    dm_list
  )
}
    
#' costs Server Function
#'
#' @noRd 
mod_costs_server <- function(input, output, session,
                             data, validateButton, resetButton){
  ns <- session$ns
  
  const_cost <- data %>%
    dplyr::filter(.data$Min == .data$Max) %>%
    .$cost
  
  varying_cost <- data %>%
    dplyr::filter(.data$Min != .data$Max)
  
  assumptions <- reactiveVal()
  
  assumptions(list(cost = data$cost))
  
  observeEvent(validateButton(), {
    assumptions(list(
      cost = c(
        purrr::map_chr(
          snakecase::to_snake_case(varying_cost$Product),~ input[[.x]]
        ),
        const_cost
      ))
    )
  })
  
  observeEvent(resetButton(), {
    purrr::map2(
      .x = snakecase::to_snake_case(varying_cost$Product),
      .y = varying_cost$cost,
      ~updateNumericInput(session, inputId = .x, value   = .y)
    )
  })
  
  return(list(choice_cost = reactive({as.numeric(assumptions()$cost)})))
}

    
## To be copied in the UI
# mod_costs_ui("costs_ui_1", data, n_sliders)
    
## To be copied in the server
# callModule(mod_costs_server, "costs_ui_1", , validateButton)
 
