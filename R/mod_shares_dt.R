#' shares_dt UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
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
mod_shares_dt_ui <- function(id){
  ns <- NS(id)
  tagList(
    DT::DTOutput(ns("SalesSharesDT")) %>%
      withSpinner()
  )
}
    
#' shares_dt Server Function
#'
#' @noRd 
mod_shares_dt_server <- function(input, output, session,
                                 data, specs, respid_key, product_names,
                                 current_prices, current_dm, current_cost,
                                 current_ms){
  ns <- session$ns
  
  output$SalesSharesDT <- DT::renderDT({
    validate(need(
      all(current_dm() <= 1 & current_dm() >= 0),
      "Please use values between 0 and 1 for Distribution margin.")
    )
  utils_shares_DT(data,
                  specs,
                  respid_key(),
                  product_names,
                  current_prices(),
                  current_dm(),
                  current_cost(),
                  current_ms())
  })
  
}
    
## To be copied in the UI
# mod_shares_dt_ui("shares_dt_ui_1")
    
## To be copied in the server
# callModule(mod_shares_dt_server, "shares_dt_ui_1", datdata = cj_utils,
# specs = specs_df,
# respid_key = current_respid$choice_respid,
# product_names = cj_specs$Product,
# current_prices = current_price$choice_price,
# current_dm = current_dm$choice_dm,
# current_cost = current_cost$choice_cost,
# current_universe = current_uni$choice_universe)
 