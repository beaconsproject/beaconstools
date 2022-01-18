library(beaconstools)

# catchments to benchmarks
test_that("benchmarks build as expected", {
  expect_snapshot(catchments_to_benchmarks(benchmark_table_sample, catchments_sample, c("PB_0001", "PB_0002")))
})

test_that("networks build as expected", {
  expect_snapshot(catchments_to_benchmarks(benchmark_table_sample, catchments_sample, c("PB_0001__PB_0002")))
})


# append reserve
test_that("reserve is added as expected", {
  benchmarks <- catchments_to_benchmarks(benchmark_table_sample, catchments_sample, c("PB_0001", "PB_0002"))
  pa_1 <- data.frame(lon = c(-85, -82.5, -83), lat = c(51, 51.5, 50.5)) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    dplyr::summarise(geometry = st_combine(geometry)) %>%
    sf::st_cast("POLYGON") %>%
    sf::st_transform(st_crs(benchmarks)) %>%
    dplyr::mutate(pa_name="a protected area", ha=9999, class="provincial")
  expect_snapshot(append_reserve(benchmarks, pa_1, "PA_1"))
})

test_that("reserve is dissolved correctly", {
  pa_1 <- catchments_to_benchmarks(benchmark_table_sample, catchments_sample, c("PB_0003", "PB_0002"))
  benchmarks <- catchments_to_benchmarks(benchmark_table_sample, catchments_sample, c("PB_0001"))
  expect_snapshot(append_reserve(benchmarks, pa_1, "PA_1"))
})

