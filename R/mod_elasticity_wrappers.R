#' @title List of Elasticities 
#' @description Calulate elasticity of shares, sales, channel sales and profit.
#'
#' @param iterated_df Data frame which is result of iter shares function.
#' 
#' @return List with calculated elasticity.
#' @export
elasticity_list_vals <- function(iterated_df) {

  e_share <- iterated_df %>%
    mutate(logx = log(.data$price/100), logy = log(.data$Share)) %>%
    stats::lm(logy~logx, data = .) %>%
    stats::coef(.) %>% 
    .[2] %>%
    round(., 2)

  e_sales <- iterated_df %>%
    mutate(logx = log(.data$price/100), logy = log(.data$Sales)) %>%
    stats::lm(logy~logx, data = .) %>%
    stats::coef(.) %>% 
    .[2] %>%
    round(., 2)

  e_ch_sales <- iterated_df %>%
    mutate(
      logx = log(.data$price/100), logy = log(.data$`Channel sales`)
    ) %>%
    stats::lm(logy~logx, data = .) %>%
    stats::coef(.) %>% 
    .[2] %>%
    round(., 2)

  e_profit <- iterated_df %>%
    mutate(logx = log(.data$price/100), logy = log(.data$Profit)) %>%
    stats::lm(logy~logx, data = .) %>%
    stats::coef(.) %>% 
    .[2] %>%
    round(., 2)
  
  return(
    list(
      e_share = e_share, e_sales = e_sales, e_ch_sales = e_ch_sales, 
      e_profit = e_profit
    )
  )

}

#' @title Make Share Elasticity Plot
#' @description Elsticity Plot of shares.
#'
#' @param iterated_df Data frame which is result of iter shares function.
#' @param product_input Product input
#' @param product_output Product output
#' @param ranges range of plot to put elasticity
#' @param e_share share elasticity value
#' 
#' @return ggplot object
#' @export

plot_e_share <- function(iterated_df,
                         product_input,
                         product_output,
                         ranges,
                         e_share) {
  e_plot <- iterated_df %>%
    ggplot(aes(x = .data$price, y = .data$Share)) +
    geom_line(size = 1.5, color ="#8c1eff") +
    theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          plot.title = element_text(size = 16, face = "bold")) +
    annotate(
      "text", x = ranges[1], y = ranges[2], color = "#ff2975", size = 6,
      label = paste("Share elasticity =", e_share),
      vjust = "inward", hjust = "inward"
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_x_continuous(labels = scales::dollar_format(
      prefix = "", suffix = "\u20ac", big.mark = ",", decimal.mark = "."
    )) +
    xlab(paste("Price: ", product_input)) +
    ylab(paste("Share: ", product_output))
  
  return(e_plot)
}

#' @title Make Sales and Profit Plot
#' @description Elasticity plot of sales, channel sales and profit.
#'
#' @param iterated_df Data frame which is result of iter shares function.
#' @param product_input Product input
#' @param product_output Product output
#' @param ranges range of plot to put elasticity
#' @param e_sales sales elasticity value
#' @param e_ch_sales channel sales elasticity value
#' @param e_profit profit elasticity value
#' 
#' @return ggplot object
#' @export

plot_e_sp <- function(iterated_df,
                      product_input,
                      product_output,
                      ranges,
                      e_sales,
                      e_ch_sales,
                      e_profit) {
  
  sp_plot <- iterated_df %>%
    select("price", "Sales", "Channel sales", "Profit") %>% #repeated, could be wrapped
    pivot_longer(
      "Sales":"Profit", names_to = "Measure", values_to = "value"
    ) %>%
    mutate(
      Measure = factor(
        .data$Measure, levels = c("Sales", "Channel sales", "Profit")
      )
    ) %>%
    ggplot(aes(x = .data$price, y = .data$value, color = .data$Measure)) +
    geom_line(size = 1.5) +
    theme_bw() +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14,face="bold"),
      plot.title = element_text(size = 16,face="bold"),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11)
    ) +
    annotate(
      "text", x = ranges[1], y = ranges[2], color = "#ff2975", size = 6,
      fontface = "bold",
      label = paste(paste("Sales elasticity =", e_sales),
      paste("Channel sales elasticity =", e_ch_sales),
      paste("Profit elasticity =", e_profit), sep = "\n"),
      vjust = "inward", hjust = "inward"
    ) +
    scale_y_continuous(
      labels = scales::dollar_format(
        prefix = "", suffix = "\u20ac", big.mark = ",", decimal.mark = "."
      )
    ) +
    scale_x_continuous(
      labels = scales::dollar_format(
        prefix = "", suffix = "\u20ac", big.mark = ",", decimal.mark = "."
      )
    ) +
    scale_color_manual(values = c("#ff901f", "#ff2975", "#8c1eff")) +
    guides(color = guide_legend(reverse = TRUE)) +
    xlab(paste("Price: ", product_input)) +
    ylab(paste("Sales & Profit: ", product_output))
  
  return(sp_plot)
}