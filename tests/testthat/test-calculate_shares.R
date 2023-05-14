context("busines logic")

test_that("calc_scaler correctly rescales to range", {
  expect_equal(calc_scaler(price_set =50, old_max = 100, old_min = 0), 0)
  expect_equal(calc_scaler(1.5, 1.5, 0), 2)
  expect_equal(calc_scaler(-10, 10, -10), -2)
  expect_equal(calc_scaler( 50, 100, 0, new_max = 4,new_min = -4), 0)
  expect_equal(calc_scaler(1.5, 1.5, 0, 4, -3.5), 4)
  expect_equal(calc_scaler(-10, 10, -10, -0.7, -4), -4)
})

test_that("make_iter makes correct number of price points", {
  
})

test_that("calc_shares correctly calculate shares", {
  
})

test_that("iter_share correctly calculate shares", {
  
})