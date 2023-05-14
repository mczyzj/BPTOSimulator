#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  
  options(spinner.color = "#f111ff",
          spinner.type = 8)
  
  ### Global Variables ###
  products_change <- BPTOSimulator::cj_specs_example1 %>%
    filter(.data$Min != .data$Max)

  filters_change <- split(
    BPTOSimulator::cj_filters_example1,
    BPTOSimulator::cj_filters_example1$filter_name
  )
  
  ### SIDEBAR CONTENT ###
  sidebar <- dashboardSidebar(
    sidebarMenu( 
      tags$style(HTML(".sidebar-menu li a { font-size: 1.2em; }")),
      menuItem("Main", tabName = "main", icon = icon("home")),
      menuItem("Simulator", tabName = "simulator", icon = icon("poll-h")),
      br(),
       actionButton(
         "validate", span(icon("redo"), "Calculate"),
         style="color: #fff; background-color: #ff2975; border-color: #8c1eff",
         width = "80%"
       ),
       actionButton(
         "reset", span(icon("redo"), "Reset assumptions"),
         style="color: #fff; background-color: #ff2975; border-color: #8c1eff",
         width = "80%"
      ),
      mod_sample_boxes_ui("sample_boxes_ui_1"),
      mod_sample_boxes_ui("sample_boxes_ui_2")
    )
  )
  
  ### BODY CONTENT ###
  body <- dashboardBody(
    tabItems(
       tabItem(tabName = "main",
               fluidRow(
                 box(
                   title = span(icon("info"), "INSTRUCTION"),
                   width = 12, solidHeader = TRUE, collapsible = TRUE,
                   withSpinner(htmlOutput("mainHTML"), type =8))
               )),
  #### Simulator tab content
    tabItem(tabName = "simulator",
            ### FILTER BOX ###
            fluidRow(
              box(
                title = span(icon("filter"), " Filters"),
                width = 12, solidHeader = TRUE, collapsible = TRUE,
                mod_filters_ui(
                  "filters_ui_1", filters_change, names(filters_change)
                )
              )
            ),
            fluidRow(
              ### ASSUMPTIONS BOX ###
              box(
                title = span(icon("sliders-h"), " Assumptions"),
                width = 12, solidHeader = TRUE, collapsible = TRUE,
                column(
                  3, h4(span(icon("euro-sign"), " Price")), 
                  shinyWidgets::chooseSliderSkin("Modern", color = "#ff2975"),
                  mod_sliders_ui(
                    "sliders_ui_1", products_change, products_change$Product
                  )
                ),
                column(
                  3, h4(span(icon("calculator")), " Cost"),
                  mod_costs_ui(
                    "costs_ui_1", products_change, products_change$Product
                  )
                ),
                column(
                  3, h4(span(icon("percent")), " Distribution margin"),
                  mod_dist_margin_ui(
                    "dist_margin_ui_1", products_change, products_change$Product
                  )
                ),
                column(
                  3, h4(span(icon("globe")), " Market size"),
                  mod_market_size_ui("market_size_ui_1")
                )
              )),
              ### CHARTS AND TABLES BOX ###
            fluidRow(
              tabBox(
                side = "left", width = 12,
                selected = "none",
                tabPanel(
                  span(icon("table"), " Shares & Sales Data Frame"),
                  mod_shares_dt_ui("shares_dt_ui_1")
                ),
                tabPanel(
                  span(icon("chart-bar")," Shares Barplot"),
                  mod_barplots_ui("shr_barplots_ui_1")
                ),
                tabPanel(
                  span(icon("chart-bar")," Sales & Profit Barplot"),
                  mod_barplots_ui("sp_barplots_ui_2")
                ),
                tabPanel(
                  span(icon("chart-line"), " Elasticity Chart"),
                  mod_elasticity_plot_ui(
                    "elasticity_plot_ui_1", n_products = products_change$Product
                  )
                ),
                tabPanel(
                  span(icon("tachometer-alt"), " Optimisation Data Frame"),
                  mod_optimize_ui(
                    "optimize_ui_1", n_products = products_change$Product
                  )
                )
              )))
  ))
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # List the first level UI elements here 
    
    dashboardPage(
      dashboardHeader(
        title = "Example BPTO Simulator",
        tags$li(a(
          href = 'http://codingmanatee.ninja',
          img(
            src = 'www/cj_logo.png',
            title = "Coding Manatee Ninja",
            height = "45px"
          ),
          style = "padding: 2px 0px 0px 0px;"),
          class = "dropdown"),
          tags$li(a(
            href = 'http://codingmanatee.ninja', 
            icon("power-off"),
            title = "Back to Apps Home"),
            class = "dropdown"
          )
        ),
      sidebar,
      body
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(ico = "favicon", ext = 'png'),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'BPTO Simulator'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

