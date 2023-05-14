#' optimize UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
mod_optimize_ui <- function(id, n_products){
  ns <- NS(id)
  tagList(
    dropdownButton(
      tags$h3("Choose products"),
      selectInput(inputId = ns('optimizeProd'),
                  label = 'Optimise for brand',
                  choices = n_products,
                  selected = n_products[1]),
      numericInput(inputId = ns("optimizeStep"),
                   label = "Number of price points",
                   value = 15,
                   min = 10,
                   max = 100),
      status = "Gear",
      circle = TRUE, icon = icon("gear"),
      size = "sm",
      tooltip = tooltipOptions(
        title = "Click to set Product and number of price points!"
      )
  ),
    DT::DTOutput(ns("optimized")) %>% 
     withSpinner()
  )
}
    
#' optimize Server Function
#'
#' @noRd 
mod_optimize_server <- function(input, output, session, 
                                data, specs, respid_key, product_names,
                                current_prices, current_dm, current_cost,
                                current_ms, validateButton){
  
     ns <- session$ns
     
     products_change <- specs %>%
       dplyr::filter(.data$Min != .data$Max)
     
     optim_config <- reactiveVal()
     optim_config(list(
       n = 15, product = products_change$Product[1]
     ))
     
     observeEvent(validateButton(), {
       optim_config(list(n = input$optimizeStep,
                         product = input$optimizeProd))
       })
     
      output$optimized <- DT::renderDT({
        validate(
          need(
            optim_config()$n >= 10 & optim_config()$n <= 100, 
            "Please select number of steps between 10 and 100"
          )
        )

      utils_optim_DT(product_input = optim_config()$product,
                     product_output = optim_config()$product,
                     data = data,
                     specs = specs,
                     respid_key = respid_key(),
                     product_names = product_names,
                     current_prices = current_prices(),
                     current_dm = current_dm(),
                     current_cost = current_cost(),
                     current_ms = current_ms(),
                     n_points = optim_config()$n)
   })
  
}
    
## To be copied in the UI
# mod_optimize_ui("optimize_ui_1", n_products)
    
## To be copied in the server
# callModule(mod_optimize_server, "optimize_ui_1", 
# current_prices = current_price$choice_price,
# current_dm = current_dm$choice_dm,
# current_cost = current_cost$choice_cost,
# current_universe = current_uni$choice_universe,
# respid_key = current_respid$choice_respid)
 
