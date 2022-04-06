test_that("defaults work", {
  reserves <- dissolve_catchments_from_table(
    catchments_sample, 
    benchmark_table_sample,
    "network")
       
  expect_equal(sum_raster_values(reserves, "network", habitat_sample)$area_km2,
               c(359.784550, 10.719990, 0.000000, 476.872355, 197.352906, 1.716174, 516.909971, 37.532377, 0.000000)
               )
})

test_that("class_vals work", {
  reserves <- dissolve_catchments_from_table(
    catchments_sample, 
    benchmark_table_sample,
    "network")
  
  expect_equal(sum_raster_values(reserves, "network", habitat_sample, c(2,3))$area_km2,
               c(10.719990, 0.000000, 197.352906, 1.716174, 37.532377, 0.000000)
               )
})

test_that("fill_zeros FALSE works", {
  reserves <- dissolve_catchments_from_table(
    catchments_sample, 
    benchmark_table_sample,
    "network")
  
  expect_equal(sum_raster_values(reserves, "network", habitat_sample, fill_zeros = FALSE)$area_km2,
               c(359.784550, 10.719990, 476.872355, 197.352906, 1.716174, 516.909971, 37.532377)
               )
})

test_that("projection error catches", {
  reserves <- dissolve_catchments_from_table(
    catchments_sample, 
    benchmark_table_sample,
    "network") %>%
    st_transform(3857)
  
  expect_error(sum_raster_values(reserves, "network", habitat_sample),
               "is not TRUE"
               )
})
