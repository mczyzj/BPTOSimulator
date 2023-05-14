context("filtering utility")

test_that("utils_filter_df correctly filters utils df", {
  sample_df <- data.frame(
    Respondent = 1:10,
    Prod1 = rnorm(10, 1, 0.2),
    att_Prod1 = rnorm(10, -1, 0.15),
    stringsAsFactors = FALSE
  )
  
  respid_key_sample   <- c(seq(1, 10, by =  2))
  respid_key_sample_2 <- 3
  respid_key_sample_3 <- c(8,12,13)
  
  test_f <- utils_filter_df(sample_df, respid_key_sample)
  test_f_2 <- utils_filter_df(sample_df, respid_key_sample_2)
  test_f_3 <- utils_filter_df(sample_df, respid_key_sample_3)
  
  expect_equal(dim(test_f)[1], 5)
  expect_equal(dim(test_f_2)[1], 1)
  expect_equal(dim(test_f_3)[1], 1)
  
  expect_equal(test_f$Respondent, c(1,3,5,7,9))
  expect_equal(test_f_2$Respondent, c(3))
  expect_equal(test_f_3$Respondent, c(8))
})
