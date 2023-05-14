#' @title Map Price To New Range
#' @description Takes a number from range and maps it to new range.
#' 
#' @param price_set Numeric. Price in current range.
#' @param old_max Numeric. Maximum price of product.
#' @param old_min Numeric. Minimal price of product.
#' @param new_max Numeric. New maximum price of product. Default `1`.
#' @param new_min Numeric. New minimum price of prouct. Default `-1`.
#'
#' @return Numeric. Price with the new value that is in the range of `new_max`
#'   and `new_min`.
#' @export



calc_scaler <- function(price_set, old_max, old_min, new_max = 1, new_min = -1) {
  if (old_max == old_min) {
    return(0)
  } else {
    scaler <- new_min + (((price_set - old_min) * (new_max - new_min)) / 
                         (old_max - old_min)) 
    return(scaler) 
  }
}


#' @title Make Iterations to Calculate Elasticity
#' @description Function to calulates steps (price points) for elasticyty
#'   simulation.
#'
#' @param product_input Character. Product for which elasticity will be calculated.
#' @param specs Data frame. Table with products specification.
#' @param n_points Numeric. Number of points for which Max-Min range will be 
#'   divided. Default `20`.
#'
#' @return Numeric Vector. A vector of prices to simulate the elasticity.
#' @export


make_iter <- function(product_input,
                      specs,
                      n_points = 20) {
  step_vector <- specs %>%
    filter(.data$Product == product_input) %>%
    mutate(step = (.data$Max - .data$Min)/n_points) %>%
    .$step * 0:n_points
  return(step_vector)
}

#' @title Calulate Shares for Elasticyty Simulation
#'
#' @description Iterates through prices of Input brand and calulates
#' shares for Output brand.
#'
#' @param product_input Input product for which elasticity will be calculated.
#' @param product_output Output product for which elasticity will be calculated.
#' @param data Data frame with hb utilities.
#' @param specs Data frame with products specification.
#' @param respid_key Vector with Respondents ID to filter.
#' @param product_names Vector of product names.
#' @param current_prices Snapshot of prices from Assumptions tab.
#' @param current_dm Snapshot of distribution margin from Assumptions tab.
#' @param current_cost Snapshot of cost from Assumptions tab.
#' @param current_ms Snapshot of market size from Assumption tab.
#' @param n_points Number of points for which Max-Min range will be divided.
#'
#'
#' @return Data frame with calculated shares by price step.
#' @export

iter_share <- function(product_input,
                       product_output,
                       data,
                       specs,
                       respid_key,
                       product_names,
                       current_prices,
                       current_dm,
                       current_cost,
                       current_ms,
                       n_points = 20) {
  
  product_no <- which(specs$Product == product_input)
  
  product_min <- specs %>%
    filter(.data$Product == product_input) %>%
    .$Min
  
  iterations <- make_iter(product_input, specs, n_points)
  
  loop_prices <- current_prices
  
  share_list <- vector("list", length = length(iterations))
  
  for (i in 1:length(iterations)) {
    loop_prices[product_no] <- iterations[i] + product_min
    share_list[[i]] <- calc_shares(data,
                                   specs,
                                   respid_key,
                                   product_names,
                                   loop_prices,
                                   current_dm,
                                   current_cost,
                                   current_ms) %>%
      filter(.data$Product %in% c(product_output)) %>%
      mutate(price = loop_prices[product_no])
  }
  
  df_output <- share_list %>%
    bind_rows()
}


#' @title Calculate Shares
#' @description Calculates shares, sales, channel sales and profit of 
#'   all products in Specs.
#'
#' @param data df with hb utilities.
#' @param specs Table with products specification.
#' @param respid_key vector with Respondents ID to filter.
#' @param product_names Vector of product names.
#' @param current_prices snapshot of prices from Assumptions tab.
#' @param current_dm snapshot of distribution margin from Assumptions tab.
#' @param current_cost snapshot of cost from Assumptions tab.
#' @param current_ms snapshot of market size from Assumption tab.
#'
#'
#' @return Data frame with calculated shares by price step.
#' @export
calc_shares <- function(data,
                        specs,
                        respid_key,
                        product_names,
                        current_prices,
                        current_dm,
                        current_cost,
                        current_ms) {
  
  ### set variables ###
  
  product_cols_n <- nrow(specs)
  current_utils <- utils_filter_df(data, respid_key) 
  
  ### loop to set scaled prices ###
  
  current_scalers <- vector("numeric", length(current_prices))
  
  for (i in 1:length(current_prices)) {
    current_scalers[i] <- calc_scaler(
      price_set = current_prices[i],
      old_max = specs[i, 4], old_min = specs[i, 2]
    )
  }
  
  ### Matrix multiplication to obtain Data Frame with 
  ### exp(util_n + util_price_n * scaler_n).
  
  mulitp_matrix <- cbind(
    diag(ncol = nrow(specs), nrow = nrow(specs), 1),
    diag(ncol = nrow(specs), nrow = nrow(specs), current_scalers)
  )
  
  current_utils <- exp(
    as.matrix(current_utils %>% select(-"Respondent")) %*% t(mulitp_matrix)
  )
  
  current_utils %>%
    prop.table(., 1) %>%
    colMeans() %>%
    as.data.frame() %>%
    setNames("Share") %>%
    mutate(Product = product_names, .before = "Share") %>%
    mutate(
      Units = .data$Share * current_ms,
      tmp_price = current_prices,
      tmp_cost = current_cost,
      tmp_dm = current_dm,
      Sales = .data$Units * .data$tmp_price,
      tmp_sum_sale = sum(.data$Sales),
      `% Sales` = .data$Sales / .data$tmp_sum_sale,
      `Channel sales` = .data$Sales * (1 - .data$tmp_dm),
      Profit = .data$`Channel sales` - (.data$Units * .data$tmp_cost)
    ) %>%
    select(-starts_with("tmp_")) 
}