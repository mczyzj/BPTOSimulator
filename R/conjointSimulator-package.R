#' @importFrom dplyr bind_rows filter mutate select
#' @importFrom htmltools tags tagAppendAttributes tagList HTML
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom rlang .data
#' @importFrom shiny NS tagList column
#' @importFrom tidyr pivot_longer
#' @import shiny
#' @import shinydashboard
#' @import shinycssloaders
#' @import shinyWidgets
#' @import DT
#' @import ggplot2
#' @import tidyselect
#' @title BPTO Simulator
#' @description This is a boilerplate for BPTO Simulator for HB conjoint models.
#'   It works with Alternative Specific design, where the Alternatives are 
#'   brands, and their only attribute is Price. The package supports the models
#'   with some alternatives having constant price (for details please visit 
#'   Vignette). To build the simulator you need to provide data in proper format,
#'   and the package will figure out itselfe the user interface. As of now, it
#'   gives the insight in Share of Preference, Revenue, Channel Sales and Profit.
#'   
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

utils::globalVariables(c("."))