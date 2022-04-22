
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
    data.frame('PA_192941' = as.integer())
  )
})