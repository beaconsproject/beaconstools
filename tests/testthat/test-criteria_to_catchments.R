library(beaconstools)
library(sf)

test_that("projection error catches", {
  expect_error(
    criteria_to_catchments(st_transform(catchments_sample, 3857), led_sample, "led"),
    "Raster projection does not match"
    )
})

test_that("CATCHNUM error catches", {
  expect_error(
    criteria_to_catchments(catchments_sample[c(2:14)], led_sample, "led"),
    "CATCHNUM"
    )
})

test_that("areas are as expected", {
  expect_snapshot_output(criteria_to_catchments(catchments_sample, led_sample, "led"))
})