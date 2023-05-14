#' filters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
mod_filters_ui <- function(id, data, n_filters){
  ns <- NS(id)
  
  tagList(
    filters_list <- vector("list", length(n_filters)),
    for (i in 1:length(n_filters)) {
      filters_list[[i]] <- column(1,
        awesomeCheckboxGroup(
          inputId = ns(snakecase::to_snake_case(n_filters[i])),
          label   = n_filters[i],
          choices = data[[i]][, 2],
          selected = data[[i]][, 2]#,
#          status = "warning"
        )
        )
    },
    filters_list
  )
}

#' filters Server Function
#'
#' @noRd 
mod_filters_server <- function(input, output, session, data, n_filters, validateButton){
  ns <- session$ns
  
  filters <- reactiveVal()
  
  filters(list(respondent = data$respondent))
  
  
  observeEvent(validateButton(), {
    respid_map <- purrr::map(snakecase::to_snake_case(n_filters), 
                          ~data[[.x]] %in% input[[.x]])
    respid_red <- purrr::reduce(respid_map, `&`)
    data_f     <- data[respid_red, ]
    filters(list(respondent = data_f$respondent))
  })
  
  return(list(choice_respid = reactive({filters()$respondent})))
}
    
## To be copied in the UI
# mod_filters_ui("filters_ui_1")
    
## To be copied in the server
# callModule(mod_filters_server, "filters_ui_1")

