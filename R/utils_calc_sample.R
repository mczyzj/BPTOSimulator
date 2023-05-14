#' Conjoint tools to calulate shares
#'
#' \code{calc_shares} calculates shares, sales, channel sales and profit.
#'
#' @param respid_key IDs from data frame with currently applied filters.
#'
#' @return Sample size.
#' @export

calc_sample <- function(respid_key) {

  current_sample <- respid_key %>%
    length()
  
  return(current_sample)
}