
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="inst/app/www/cj_logo.png" width="250px" align= "right" />

# BPTOSimulator

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

The BPTOSimulator is a `Shiny` package build with `golem` framework.
It’s purpose it to allow easy setup of BPTO Conjoint Simulator based on
Hierarchical Bayes Models. The simulator part of dashboard is build
automatically from the data provided. Quick adjustments to Interface
Layout might be provided with `custom.css` file.

## Installation

- You can install the boilerplate directly form
  [GitHub](https://github.com/mczyzj/BPTOSimulator) and play with the
  example.

``` r
# install.packages("remotes")
remotes::install_github("mczyzj/BPTOSimulator")
```

- The Simulator works as a package. It means that to make it fully
  operational you need to download (clone) the repository, and tailor it
  to your needs.

## Adjustments

There are few things you will need or like to adjust:

- The data set shouls be saved as package data. You will also need to
  put correct names of your data sets in `app_ui` and `app_server`

``` r
### ADJUST THIS LINES WITH YOUR DATA SET NAMES
### app_server.R
respid_filter <- BPTOSimulator::cj_key_filters_example1 %>%
  stats::setNames(snakecase::to_snake_case(colnames(.)))
filter_names <- unique(BPTOSimulator::cj_filters_example1$filter_name)
specs_df <- BPTOSimulator::cj_specs_example1
utils_df <- BPTOSimulator::cj_utils_example1
### app_ui.R
products_change <- BPTOSimulator::cj_specs_example1 %>%
    filter(.data$Min != .data$Max)

filters_change <- split(
  BPTOSimulator::cj_filters_example1,
  BPTOSimulator::cj_filters_example1$filter_name
)
```

- The overall layout. Most of the elements can be quickly adjusted in
  `inst/app/www/custom.css` file. Also in the `inst/app/www` folder you
  will find *logo* and *favicon* you will like to change.
