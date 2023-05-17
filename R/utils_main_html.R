#' @title Main Tab HTML
#' @description  Create main tab html.
#'
#' @param simulator_name Name of the simulator to be displayed.
#'
#' @return HTML for Main Tab.
#' @export

main_tab_html <- function(simulator_name) {
  str0 <- paste0(
    '<h2 style = "color:#ff2975">', "<b>", simulator_name,"</b></h2>"
  )
  str1 <- paste0(
    '<h4 style = "color:#ff2975">', icon("table"), "<b> Main tab:</b></h4>"
  )
  str2 <- paste(
    icon("cog", class = "icon-violet"), "Instruciton..."
  )
  str3 <- paste(
    icon("cog", class = "icon-violet"), "Switch to ", icon("chart-bar"),
    "<b>Simulator tab</b> to see results and set up assumptions."
  )
  str4 <- paste0(
    '<h4 style = "color:#ff2975">', icon("chart-bar"), 
    "<b> Simulator tab:</b></h4>"
  )
  str5 <- paste(
    icon("cog", class = "icon-violet"),
    "Scroll bars are used to set the prices for Products."
  )
  str5a <- paste(
    icon("cog", class = "icon-violet"),
    "Use the checkboxes in", icon("filter", class = "icon-violet"),
    "<b>Filters</b> tab to select subsample. Conjoint simulation will be made on the sub-sample with size shown in side-menu."
  )
  str6 <- paste(
    icon("cog", class = "icon-violet"),
    "<b>Price Reset</b> button allows all prices to return to the base (or reference) price."
  )
  str7 <- paste(
  icon("cog", class = "icon-violet"), 
    "An assumption about the total applied area that study sample represents for a group can be made in the", 
    icon("globe", class = "icon-violet"), "<b>Market size</b> input. This helps estimate the magnitude of sales and profit."
  )
  str8 <- paste(
    icon("cog", class = "icon-violet"), 
    "Channel margin and unit cost of products can be adjusted in the",
    icon("sliders-h", clas = "icon-violet"), "<b>Assumptions tab</b>."
  )
  str9 <- paste0(
    '<h4 style = "color:#ff2975">', icon("chart-line"),
    "<b> Price Sensitivity Chart:</b></h4>"
  )
  str10 <- paste(
    icon("chart-line", class = "icon-violet"), 
    "<b>Price Sensitivity Chart</b> allows observation of how the market share of a product",
    "responds to the change of either its own price or the price of another single product."
  )
  str11 <- paste(
    icon("cog", class = "icon-violet"),
    "Use <b>Product Outcome</b> dropdown box to select the product that you want to see its market share change (<i>y-axis</i>).",
    "Use <b>Product Change</b> dropdown box to select the product that changes the price (<i>x-axis</i>)."
  )
  str12 <- paste(
    icon("cog", class = "icon-violet"), 
    "The price sensitivity curve observed is a snapshot of the relationship when",
    "the prices of all other products stay as they are currently set in the simulator."
  )
  str13 <- paste(
    icon("cog", class = "icon-violet"),
    "After changing the price of any product in the Control section, the price",
    "sensitivity curve <b>does NOT update automatically</b>.",
    "Make sure to click the <b>Update Chart</b> button before using the price sensitivity chart."
  )
  str14 <- paste0(
    '<h4 style = "color:#ff2975">', icon("sliders-h"), "<b> Assumptions tab</b></h4>"
  )
  str15 <- paste(
    icon("cog", class = "icon-violet"),
    "Assumptions on distribution margin and cost per hectare by country can made by",
    "modifying the values in the yellow shaded cells in the", 
    icon("sliders-h", class = "icon-violet"), "<b>Assumptions tab</b>."
  )
  html_main <- HTML(paste(
    str0, str1, str2, str3, "<br/>", 
    str4, str5a, str5, str6, str7, str8, "<br/>",
    str9, str10, str11, str12, str13, "<br/>",
    str14, str15, sep = "<br/>"))
  return(html_main)
}
