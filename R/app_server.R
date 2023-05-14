#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @noRd
app_server <- function( input, output, session ) {

  ### Global variables
  respid_filter <- BPTOSimulator::cj_key_filters_example1 %>%
    stats::setNames(snakecase::to_snake_case(colnames(.)))
  filter_names <- unique(BPTOSimulator::cj_filters_example1$filter_name)
  specs_df <- BPTOSimulator::cj_specs_example1
  utils_df <- BPTOSimulator::cj_utils_example1
  
   instruction_tab <- main_tab_html("Example of BPTO Study")
   
   output$mainHTML <- renderUI({instruction_tab})
  
  ### Retrive assumption settings ###
   current_price <- callModule(
     mod_sliders_server, "sliders_ui_1", specs_df, 
     reactive(input$validate), reactive(input$reset)
   )
   current_cost <- callModule(
     mod_costs_server, "costs_ui_1", specs_df,
     reactive(input$validate), reactive(input$reset)
   )
   current_dist_margin <- callModule(
     mod_dist_margin_server, "dist_margin_ui_1", specs_df,
     reactive(input$validate), reactive(input$reset)
   )
   current_ms <- callModule(
     mod_universe_server, "market_size_ui_1",
     reactive(input$validate)
   )
   current_respid <- callModule(
     mod_filters_server, "filters_ui_1",
     respid_filter, filter_names,
     reactive(input$validate)
  )
  
  ### Render Value boxes with sample size and total size ###
  callModule(mod_sample_boxes_server, "sample_boxes_ui_1",
             data = utils_df,
             respid_key = current_respid$choice_respid,
             type = "total")

  callModule(mod_sample_boxes_server, "sample_boxes_ui_2",
             data = utils_df,
             respid_key = current_respid$choice_respid,
             type = "sample")

  ### Render Sales and Shares DT ###
  callModule(mod_shares_dt_server, "shares_dt_ui_1",
             data             = utils_df,
             specs            = specs_df,
             respid_key       = current_respid$choice_respid,
             product_names    = specs_df$Product,
             current_prices   = current_price$choice_price,
             current_dm       = current_dist_margin$choice_dm,
             current_cost     = current_cost$choice_cost,
             current_ms       = current_ms$choice_ms)

  ### Render Shares Barplot ###
  callModule(mod_barplots_server, "shr_barplots_ui_1",
             data             = utils_df,
             specs            = specs_df,
             respid_key       = current_respid$choice_respid,
             product_names    = specs_df$Product,
             current_prices   = current_price$choice_price,
             current_dm       = current_dist_margin$choice_dm,
             current_cost     = current_cost$choice_cost,
             current_ms       = current_ms$choice_ms,
             type = "Share")

  ### Render Sales and Profit Barplot ###
  callModule(mod_barplots_server, "sp_barplots_ui_2",
             data             = utils_df,
             specs            = specs_df,
             respid_key       = current_respid$choice_respid,
             product_names    = specs_df$Product,
             current_prices   = current_price$choice_price,
             current_dm       = current_dist_margin$choice_dm,
             current_cost     = current_cost$choice_cost,
             current_ms       = current_ms$choice_ms,
             type = "Sales")

  ### Render Optimized DT ###
  callModule(mod_optimize_server, "optimize_ui_1",
             data             = utils_df,
             specs            = specs_df,
             respid_key       = current_respid$choice_respid,
             product_names    = specs_df$Product,
             current_prices   = current_price$choice_price,
             current_dm       = current_dist_margin$choice_dm,
             current_cost     = current_cost$choice_cost,
             current_ms       = current_ms$choice_ms,
             reactive(input$validate))

  ### Render elasticity plot ###
  callModule(mod_elasticity_plot_server, "elasticity_plot_ui_1",
             data             = utils_df,
             specs            = specs_df,
             respid_key       = current_respid$choice_respid,
             product_names    = specs_df$Product,
             current_prices   = current_price$choice_price,
             current_dm       = current_dist_margin$choice_dm,
             current_cost     = current_cost$choice_cost,
             current_ms       = current_ms$choice_ms,
             reactive(input$validate))
}
