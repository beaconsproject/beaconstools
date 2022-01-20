library(beaconstools)

# catchments to benchmarks
test_that("benchmarks build as expected", {
  expect_snapshot(catchments_to_benchmarks(benchmark_table_sample, catchments_sample, c("PB_0001", "PB_0002")))
})

test_that("networks build as expected", {
  expect_snapshot(catchments_to_benchmarks(benchmark_table_sample, catchments_sample, c("PB_0001__PB_0002")))
})

test_that("CATCHNUM error catches", {
  expect_error(
    catchments_to_benchmarks(benchmark_table_sample, catchments_sample[colnames(catchments_sample) != "CATCHNUM"], c("PB_0001__PB_0002")),
    "CATCHNUM"
  )
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

test_that("network error catches", {
  pa_1 <- catchments_to_benchmarks(benchmark_table_sample, catchments_sample, c("PB_0003", "PB_0002"))
  benchmarks <- catchments_to_benchmarks(benchmark_table_sample, catchments_sample, c("PB_0001"))
  names(benchmarks) <- c("networks", "geometry")
  expect_error(
    append_reserve(benchmarks, pa_1, "PA_1"),
    "network"
  )
})


# benchmarks_to_networks
test_that("network error catches", {
  benchmarks <- catchments_to_benchmarks(benchmark_table_sample, catchments_sample, colnames(benchmark_table_sample))
  names(benchmarks) <- c("networks", "geometry")
  expect_error(
    benchmarks_to_networks(benchmarks, "PB_0001__PB_0002"),
    "network"
  )
})

test_that("geometry error catches", {
  benchmarks <- catchments_to_benchmarks(benchmark_table_sample, catchments_sample, colnames(benchmark_table_sample))
  names(benchmarks) <- c("network", "geom")
  expect_error(
    benchmarks_to_networks(benchmarks, "PB_0001__PB_0002"),
    "geometry"
  )
})

test_that("benchmarks name error catches", {
  benchmarks <- catchments_to_benchmarks(benchmark_table_sample, catchments_sample, colnames(benchmark_table_sample))
  expect_error(
    benchmarks_to_networks(benchmarks, "PB_0001__PB_0004"),
    "PB_0004"
  )
})

test_that("networks made as expected", {
  benchmarks <- catchments_to_benchmarks(benchmark_table_sample, catchments_sample, colnames(benchmark_table_sample))
  expect_snapshot(benchmarks_to_networks(benchmarks, c("PB_0001__PB_0002", "PB_0001__PB_0003")))
})

test_that("benchmarks_to_networks results matches catchments_to_benchmarks", {
  # geometries are different in the following object even though they describe the exact same area. 
  # I assume the list of points in the geometry value is different depending on the source polygons that were st_unioned (i.e. catchments vs benchmarks)
  # instead test the areas
  benchmarks <- catchments_to_benchmarks(benchmark_table_sample, catchments_sample, colnames(benchmark_table_sample))
  expect_equal(
    sf::st_area(catchments_to_benchmarks(benchmark_table_sample, catchments_sample, c("PB_0001__PB_0002", "PB_0001__PB_0003"))$geometry),
    sf::st_area(benchmarks_to_networks(benchmarks, c("PB_0001__PB_0002", "PB_0001__PB_0003"))$geometry)
  )
})