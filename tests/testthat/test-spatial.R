library(beaconstools)

test_that("benchmarks build as expected", {
  expect_snapshot(catchments_to_networks(benchmark_table_sample, catchments_sample, c("PB_0001", "PB_0002")))
})

test_that("networks build as expected", {
  expect_snapshot(catchments_to_networks(benchmark_table_sample, catchments_sample, c("PB_0001__PB_0002")))
})

