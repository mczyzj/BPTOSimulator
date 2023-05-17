#' @title Calculate Sample Size
#' @description Calculates the sample size based on filters.
#'
#' @param respid_key IDs from data frame with currently applied filters.
#'
#' @return Numeric. Sample size.
#' @export

calc_sample <- function(respid_key) {

  current_sample <- respid_key %>%
    length()
  
  return(current_sample)
}