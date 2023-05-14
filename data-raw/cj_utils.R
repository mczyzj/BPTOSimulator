## code to prepare `cj_*` datasets goes here

cj_utils_example1 <- read.csv("data-raw/cj_utils_example1.csv", stringsAsFactors = FALSE)
cj_filters_example1 <- read.csv("data-raw/cj_filters_example1.csv", stringsAsFactors = FALSE)
cj_key_filters_example1 <- read.csv("data-raw/cj_key_filters_example1.csv", stringsAsFactors = FALSE)
cj_specs_example1 <- read.csv("data-raw/cj_specs_example1.csv", stringsAsFactors = FALSE)
cj_specs_test_example1 <- read.csv("data-raw/cj_specs_example1.csv", stringsAsFactors = FALSE) %>%
  dplyr::mutate(Max = ifelse(Product %in% c("Competitor 5", "Competitor 6"), Min, Base),
                Base = ifelse(Product %in% c("Competitor 5", "Competitor 6"), Min, Base),
                dm = ifelse(Product %in% c("Competitor 5", "Competitor 6"), 0, dm),
                cost = ifelse(Product %in% c("Competitor 5", "Competitor 6"), 0, cost))


cj_utils_example2 <- read.csv("data-raw/cj_utils_example2.csv", stringsAsFactors = FALSE)
cj_filters_example2 <- read.csv("data-raw/cj_filters_example2.csv", stringsAsFactors = FALSE)
cj_key_filters_example2 <- read.csv("data-raw/cj_key_filters_example2.csv", stringsAsFactors = FALSE)
cj_specs_example2 <- read.csv("data-raw/cj_specs_example2.csv", stringsAsFactors = FALSE)

usethis::use_data(cj_utils_example1, overwrite = TRUE)
usethis::use_data(cj_filters_example1, overwrite = TRUE)
usethis::use_data(cj_key_filters_example1, overwrite = TRUE)
usethis::use_data(cj_specs_example1, overwrite = TRUE)
usethis::use_data(cj_specs_test_example1, overwrite = TRUE)

usethis::use_data(cj_utils_example2, overwrite = TRUE)
usethis::use_data(cj_filters_example2, overwrite = TRUE)
usethis::use_data(cj_key_filters_example2, overwrite = TRUE)
usethis::use_data(cj_specs_example2, overwrite = TRUE)
