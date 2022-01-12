library(beaconstools)

test_that("targets are as expected with 5000 reserve size", {
  expect_snapshot(gen_targets(catchments_sample, led_sample, 5000))
})

test_that("targets are as expected with 10000 reserve size", {
  expect_snapshot(gen_targets(catchments_sample, led_sample, 10000))
})

test_that("targets are as expected with class_vals", {
  expect_snapshot(gen_targets(catchments_sample, led_sample, 10000, c(1:5)))
})