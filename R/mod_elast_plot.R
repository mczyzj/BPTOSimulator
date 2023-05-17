#' @title Elasticity Plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param names_products Names of products for which there is price range in 
#'   Specification.
#' @param data Data frane with HB utilities.
#' @param specs Table with products specification.
#' @param respid_key Vector with Respondents ID to filter.
#' @param product_names Vector of product names.
#' @param current_prices Snapshot of prices from Assumptions tab.
#' @param current_dm Snapshot of distribution margin from Assumptions tab.
#' @param current_cost Snapshot of cost from Assumptions tab.
#' @param current_ms Snapshot of market size from Assumption tab.
#' @param validateButton Object that triggers validation.
#' 
#' @noRd 
mod_elasticity_plot_ui <- function(id, names_products){
  ns <- NS(id)
  tagList(
     dropdownButton(
       tags$h3("Choose products"),
       selectInput(inputId = ns('elasticityX'),
                   label = 'Product input',
                   choices = names_products,
                   selected = names_products[1]),
       selectInput(inputId = ns('elasticityY'),
                   label = 'Product outcome',
                   choices = names_products,
                   selected = names_products[1]),
       awesomeRadio(inputId = ns("chartSelect"),
                    label = h4("Choose what to plot"),
                    choices = c("Shares", "Sales and Profit"),
                    selected = "Shares",#
                    #status = "warning",
                    inline = TRUE,
                    checkbox = TRUE),
       status = "Gear",
       circle = TRUE, icon = icon("gear"),
       size = "sm",
       tooltip = tooltipOptions(
         title = "Click to change input, output and chart type!"
      )
    ),
    plotOutput(ns("elasticityChart"))
  )
}
    
#' elasticity_plot Server Function
#'
#' @noRd 
mod_elasticity_plot_server <- function(input, output, session,
                                       data, specs, respid_key, product_names,
                                       current_prices, current_dm, current_cost,
                                       current_ms, validateButton){
  ns <- session$ns
  
  products_change <- specs %>%
    dplyr::filter(.data$Min != .data$Max)
  
  optim_config <- reactiveVal()
  optim_config(list(product_input = products_change$Product[1],
                    product_output = products_change$Product[1],
                    type = "Shares"
  ))

  observeEvent(validateButton(), {
    optim_config(list(product_input = input$elasticityX,
                      product_output = input$elasticityY,
                      type = input$chartSelect))
  })
  
  output$elasticityChart <- renderPlot({
    
    df_elasticity <- iter_share(
      product_input = optim_config()$product_input,
      product_output = optim_config()$product_output,
      data = data,
      specs = specs,
      respid_key = respid_key(),
      product_names = product_names,
      current_prices = current_prices(),
      current_dm = current_dm(),
      current_cost = current_cost(),
      current_ms = current_ms(),
      n_points = 20
    )
    
    list_elasticity <- elasticity_list_vals(df_elasticity)
    
    ranges <- c(min(df_elasticity$price), min(df_elasticity$Share))
    
    if (optim_config()$type == "Shares") {

      plot_e_share(
        iterated_df = df_elasticity,
        product_input = optim_config()$product_input,
        product_output = optim_config()$product_output,
        ranges = ranges,
        e_share = list_elasticity$e_share
      )
      
    
   } else if (optim_config()$type == "Sales and Profit"){
     
   
     plot_e_sp(iterated_df = df_elasticity,
               product_input = optim_config()$product_input,
               product_output = optim_config()$product_output,
               ranges = ranges,
               e_sales = list_elasticity$e_sales,
               e_ch_sales = list_elasticity$e_ch_sales,
               e_profit = list_elasticity$e_profit)
   }
    })
  
}
    
## To be copied in the UI
# mod_elasticity_plot_ui("elasticity_plot_ui_1", n_products)
    
## To be copied in the server
# callModule(mod_elasticity_plot_server, "elasticity_plot_ui_1")
 
