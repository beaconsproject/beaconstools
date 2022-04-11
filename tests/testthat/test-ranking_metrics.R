# sum_raster_values
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

# sum_polygon_values
test_that("defaults work", {
  reserves <- dissolve_catchments_from_table(
    catchments_sample, 
    benchmark_table_sample,
    "network")
  habitat <- catchments_sample %>%
    dplyr::filter(CATCHNUM %in% c(191396, 191649)) %>%
    dplyr::mutate(group = c(1,2)) %>%
    dplyr::select(group)
  
  expect_equal(sum_polygon_values(reserves, "network", habitat)$area_km2,
               c(173.664, 0.000, 179.236)
  )
})

test_that("groups work", {
  reserves <- dissolve_catchments_from_table(
    catchments_sample, 
    benchmark_table_sample,
    "network")
  habitat <- catchments_sample %>%
    dplyr::filter(CATCHNUM %in% c(191396, 191649)) %>%
    dplyr::mutate(group = c(1,2)) %>%
    dplyr::select(group)
  
  expect_equal(sum_polygon_values(reserves, "network", habitat, "group")$area_km2,
               c(173.664, 0.000, 0.000, 0.000, 173.664, 5.572)
  )
})

test_that("class_vals work", {
  reserves <- dissolve_catchments_from_table(
    catchments_sample, 
    benchmark_table_sample,
    "network")
  habitat <- catchments_sample %>%
    dplyr::filter(CATCHNUM %in% c(191396, 191649)) %>%
    dplyr::mutate(group = c(1,2)) %>%
    dplyr::select(group)
  
  expect_equal(sum_polygon_values(reserves, "network", habitat, "group", c(1))$area_km2,
               c(173.664, 0.000, 173.664)
  )
})

test_that("fill_zeros FALSE works with groups", {
  reserves <- dissolve_catchments_from_table(
    catchments_sample, 
    benchmark_table_sample,
    "network")
  habitat <- catchments_sample %>%
    dplyr::filter(CATCHNUM %in% c(191396, 191649)) %>%
    dplyr::mutate(group = c(1,2)) %>%
    dplyr::select(group)
  
  expect_equal(sum_polygon_values(reserves, "network", habitat, "group", fill_zeros = FALSE)$area_km2,
               c(173.664, 173.664, 5.572)
  )
})

test_that("fill_zeros FALSE works without groups", {
  reserves <- dissolve_catchments_from_table(
    catchments_sample, 
    benchmark_table_sample,
    "network")
  habitat <- catchments_sample %>%
    dplyr::filter(CATCHNUM %in% c(191396, 191649)) %>%
    dplyr::mutate(group = c(1,2)) %>%
    dplyr::select(group)
  
  expect_equal(sum_polygon_values(reserves, "network", habitat, fill_zeros = FALSE)$area_km2,
               c(173.664, 179.236)
  )
})

test_that("projection error catches", {
  reserves <- dissolve_catchments_from_table(
    catchments_sample, 
    benchmark_table_sample,
    "network") %>%
    st_transform(3857)
  habitat <- catchments_sample %>%
    dplyr::filter(CATCHNUM %in% c(191396, 191649)) %>%
    dplyr::mutate(group = c(1,2)) %>%
    dplyr::select(group)
  
  expect_error(sum_polygon_values(reserves, "network", habitat),
               "is not TRUE"
  )
})
