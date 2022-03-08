test_that("dci works", {
  reserves <- catchments_to_benchmarks(benchmark_table_sample, catchments_sample, c("PB_0001", "PB_0002"))
  expect_equal(calc_dci(reserves, streams_sample),
               c(0.123,0.296))
})

test_that("lwdci works", {
  reserves <- catchments_to_benchmarks(benchmark_table_sample, catchments_sample, c("PB_0001", "PB_0002"))
  streams_sample_edit <- streams_sample
  streams_sample_edit$BASIN[seq(1, nrow(streams_sample_edit), by=2)] <- 999
  expect_equal(calc_lwdci(reserves, streams_sample_edit),
               c(0.148,0.200))
})

test_that("zero assignment works when no streams", {
  reserves <- catchments_to_benchmarks(benchmark_table_sample, catchments_sample, c("PB_0001", "PB_0002"))
  streams_sample_edit <- streams_sample[1:1400,]
  expect_equal(calc_dci(reserves, streams_sample_edit),
               c(0.123,0.000))
})
