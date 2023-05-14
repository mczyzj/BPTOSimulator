#' @title DT Table of Optimized Shares, Sales and Profit
#' @description Creates DT optimized table with shares, sales and profit.
#'
#' @param product_input Product for optimization.
#' @param brand_output Product for optimization.
#' @param data Data frame with hb utilities.
#' @param specs Data frame with specs.
#' @param respid_key Vector of respondents IDs.
#' @param product_names Vector of product names.
#' @param current_prices Vector of prices.
#' @param current_dm Vector of distribution margins.
#' @param current_cost Vector of costs.
#' @param current_ms Market size.
#' @param n_points Number of price points to optimize.
#' 
#' @return Data frame with optimized shares, sales and profit.
#' @export

utils_optim_DT <- function(product_input,
                           product_output,
                           data,
                           specs,
                           respid_key,
                           product_names,
                           current_prices,
                           current_dm,
                           current_cost,
                           current_ms,
                           n_points) {
  
  df_optimized <- iter_share(product_input, product_output,
                             data, specs,
                             respid_key, product_names,
                             current_prices, current_dm, current_cost,
                             current_ms, n_points) %>%
    dplyr::select(
      "Price" = "price", "Share", "Sales", "Channel sales", "Profit"
    ) %>%
    datatable(
      caption = paste0("Optimize: ", product_input, ", with ",
                       n_points, " steps between minimum and maximum price."),
      extensions = c("Responsive", 'FixedHeader', "Buttons", 'Scroller'),
      options = list(dom = "Bt",
                     fixedHeader = TRUE,
                     buttons =
                       list('copy', list(
                         extend = 'collection',
                         buttons = c('csv', 'excel', 'pdf'),
                         text = 'Download'
                       )),
                     deferRender = TRUE,
                     scrollY = 500,
                     scroller = TRUE),
      rownames = FALSE
    ) %>%
    dt_common_formats()  %>% #from mod_shares_dt_utils_wrapper
    formatCurrency("Price", '\U20AC', digits = 2, before = FALSE)
  
  return(df_optimized)
}