#' @title Filter Utils Data Frame
#' @description Returns filtered data frame with utilities based on choosen 
#'   filters.
#'
#' @param data Data frame with HB model utilities.
#' @param respid_key Vector of respondents IDs.
#'
#' @return Data frame with filtered records.
#' @export

utils_filter_df <- function(data, respid_key) {
  filtered_df <- data %>%
    filter(.data$Respondent %in% respid_key)
  return(filtered_df)
}
