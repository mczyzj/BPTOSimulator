#' Conjoint wrapper to change NULL to zero.
#'
#' \code{null_to_zero} changes null values to 0.
#'
#' @param data numeric or NULL value#'
#'
#' @return 0 or \code{data} value.
#' @export
 

null_to_zero <- function(data) ifelse(is.null(data), 0, data)