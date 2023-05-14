#' @title Table for Plots
#' @description Creates table to be used by plotting functions.
#'
#' @param data Data frame with utilities.
#' @param specs Data frame with products specification.
#' @param respid_key Vector with Respondents ID to filter.
#' @param product_names Vector of product names.
#' @param current_prices Prices from Assumptions tab.
#' @param current_dm Distribution margin from Assumptions tab.
#' @param current_cost Cost from Assumptions tab.
#' @param current_ms Market size from Assumption tab.
#'
#'
#' @return Data frame.
#' @export

table_to_plot <- function(data,
                          specs,
                          respid_key,
                          product_names,
                          current_prices,
                          current_dm,
                          current_cost,
                          current_ms) {
  tab_bars <- calc_shares(
    data, specs, respid_key, product_names, current_prices, current_dm,
    current_cost, current_ms
  ) %>%
    select("Product", "Share", "Sales", "Channel sales", "Profit") %>%
    pivot_longer(
      "Share":"Profit", names_to = "Measure", values_to = "value"
    ) %>%
    mutate(Product_f = factor(.data$Product, levels = product_names))
  
  return(tab_bars)
}