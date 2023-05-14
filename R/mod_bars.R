#' barplots UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_barplots_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("chart")) %>%
      withSpinner()
  )
}
    
#' barplots Server Function
#'
#' @noRd 
mod_barplots_server <- function(input, output, session,
                                data, specs, respid_key, product_names,
                                current_prices, current_dm, current_cost,
                                current_ms, type = "Shares"){
  ns <- session$ns
  
  if (type == "Sales") {
    output$chart <- renderPlot(
      table_to_plot(
        data,
        specs,
        respid_key(),
        product_names,
        current_prices(),
        current_dm(),
        current_cost(),
        current_ms()
      ) %>%
      filter(.data$Measure != "Share") %>%
      mutate(
        Measure = factor(
          .data$Measure, levels = c("Sales", "Channel sales", "Profit")
        )
      ) %>%
      ggplot(aes(
        x = stats::reorder(.data$Product_f, dplyr::desc(.data$Product_f)),
        y = .data$value,
        fill = .data$Measure
      )) +
      geom_bar(
        stat = "identity", position = position_dodge(width = 0.7), 
        color = "#ffffff"
      ) +
      theme_bw() +
      scale_fill_manual(values = c("#ff901f", "#ff2975", "#8c1eff")) +
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11)
      ) +
      scale_y_continuous(labels = scales::dollar_format(
        prefix = "", suffix = "\u20ac", big.mark = ",", decimal.mark = "."
      )) +
      coord_flip() +
      guides(fill = guide_legend(reverse = TRUE)) +
      ggtitle("Sales, Channel sales and Profit of Products") +
      xlab("Product") +
      ylab(NULL)
    )
  } else {
    output$chart <- renderPlot(
      
      table_to_plot(
        data,
        specs,
        respid_key(),
        product_names,
        current_prices(),
        current_dm(),
        current_cost(),
        current_ms()
      ) %>%
        dplyr::filter(.data$Measure == "Share") %>%
        ggplot(aes(
          x = stats::reorder(.data$Product_f, dplyr::desc(.data$Product_f)),
          y = .data$value
        )) +
        geom_bar(
          stat = "identity", position = position_dodge(width = 0.5), 
          fill = "#ff2975"
        ) +
        theme_bw() +
        theme(
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face="bold"),
          plot.title = element_text(size = 16, face="bold")
        ) +
        scale_y_continuous(labels = scales::percent_format()) +
        coord_flip() +
        ggtitle("% of market") +
        xlab("Product") +
        ylab("% Shares")
    )
  }
}

## To be copied in the UI
# mod_barplots_ui("barplots_ui_1")

## To be copied in the server
# callModule(mod_barplots_server, "barplots_ui_1", datdata = cj_utils,
# specs = specs_df,
# respid_key = current_respid$choice_respid,
# product_names = cj_specs$Product,
# current_prices = current_price$choice_price,
# current_dm = current_dm$choice_dm,
# current_cost = current_cost$choice_cost,
# current_universe = current_uni$choice_universe))

