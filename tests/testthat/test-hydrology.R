# neighbours
test_that("neighbours are as expected", {
  expect_snapshot_output(
    as.data.frame(neighbours(catchments_sample))
    )
})

# get upstream catchments
test_that("upstream catchments correctly identified", {
  pa_sf <- catchments_sample[catchments_sample$CATCHNUM %in% c(199392, 191396, 191337),"CATCHNUM"]
  names(pa_sf)[1] <- "PA"
  
  expect_snapshot_output(
    as.data.frame(get_upstream_catchments(pa_sf = pa_sf, pa_id = "PA", catchments_sf = catchments_sample))
  )
})

test_that("no upstream catchments", {
  pa_sf <- catchments_sample[catchments_sample$CATCHNUM %in% c(192941),"CATCHNUM"]
  names(pa_sf)[1] <- "PA"
  pa_sf$PA <- "PA_192941"
  
  # expect an empty tibble with correct colname
  expect_equal(
    as.data.frame(get_upstream_catchments(pa_sf = pa_sf, pa_id = "PA", catchments_sf = catchments_sample)),
    data.frame('PA_192941' = as.character())
  )
})

# dissolve upstream catchments - km2 and AWI
test_that("upstream catchments dissolve correctly", {
  pa_sf <- catchments_sample[catchments_sample$CATCHNUM %in% c(199392, 191396, 191337),"CATCHNUM"]
  names(pa_sf)[1] <- "PA"
  upstream_table <- get_upstream_catchments(pa_sf = pa_sf, pa_id = "PA", catchments_sf = catchments_sample)
  
  catchments_sample_b <- catchments_sample
  catchments_sample_b$intactness <- c(rep(0.5, 40), rep(0.9, 47))
  
  expect_snapshot_output(
  as.data.frame(dissolve_upstream_catchments(catchments_sample_b, upstream_table, "PA", intactness_id = "intactness"))
  )
})
