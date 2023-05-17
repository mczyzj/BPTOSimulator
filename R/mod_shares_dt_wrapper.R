#' @title Shares and Sales DT Wrapper
#' @description Wrapper to create DT table with shares and sales.
#'
#' @param data Data frame with hb utilities.
#' @param specs Data frame with specs.
#' @param respid_key Vector of respondents IDs.
#' @param product_names Vector of product names.
#' @param current_prices Vector of prices.
#' @param current_dm Vector of distribution margins.
#' @param current_cost Vector of costs.
#' @param current_ms Market size.
#' 
#' @return DT table with filtered records.
#' @export

utils_shares_DT <- function(data,
                            specs,
                            respid_key,
                            product_names,
                            current_prices,
                            current_dm,
                            current_cost,
                            current_ms) {
  
  shares_DT <- calc_shares(
    data, specs, respid_key, product_names, current_prices, current_dm, 
    current_cost, current_ms
  ) %>%
    mutate(Units = round(.data$Units, 0)) %>%
    datatable(
      extensions = c("Responsive", 'FixedHeader', "Buttons", 'Scroller'),
      options    = list(
        dom = "Bt",
        columnDefs = list(list(className = 'dt-right', targets = 1:6)),
        fixedHeader = TRUE,
        buttons = list(
          'copy', list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = 'Download'
        )),
        deferRender = TRUE,
        scrollY = 500,
        scroller = TRUE
      ),
      rownames = FALSE
    ) %>%
  dt_common_formats() %>%
  formatPercentage(c("% Sales"), 1) %>%
  formatStyle("Product", fontWeight = "Bold")

  return(shares_DT)
}

#' @title DT Formating Wrapper
#' @description A wrapper to format DT table currency and percents.
#'
#' @param data DT data frame
#'
#' @return Data frame with filtered records.
#' @export
 
 
dt_common_formats <- function(data) {
  dt_formats <- data %>%
    formatPercentage(c("Share"), 1) %>%
    formatCurrency(
      c("Sales", "Channel sales", "Profit"),
      '\U20AC', digits = 0, before = FALSE
    )
  
  return(dt_formats)
} 