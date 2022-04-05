test_that("BC works", {
  reserves <- dissolve_catchments_from_table(catchments_sample, benchmark_table_sample, "network", dissolve_list = c("PB_0001", "PB_0002", "PB_0003"))
  expect_equal(calc_dissimilarity(reserves, ref_poly, led_sample, 'categorical'),
               c(0.098, 0.140, 0.115))
})

test_that("KS works", {
  reserves <- dissolve_catchments_from_table(catchments_sample, benchmark_table_sample, "network", dissolve_list = c("PB_0001", "PB_0002", "PB_0003"))
  expect_equal(calc_dissimilarity(reserves, ref_poly, led_sample, 'continuous'),
               c(0.052, 0.138, 0.087))
})

test_that("BC class_vals works", {
  reserves <- dissolve_catchments_from_table(catchments_sample, benchmark_table_sample, "network", dissolve_list = c("PB_0001", "PB_0002", "PB_0003"))
  expect_equal(calc_dissimilarity(reserves, ref_poly, led_sample, 'categorical', c(1,2,3)),
               c(0.064, 0.080, 0.012))
})

# difficult to test plots
