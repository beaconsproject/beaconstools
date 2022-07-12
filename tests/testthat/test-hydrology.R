
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
    data.frame(PA_192941 = as.integer())
  )
})

# get downstream catchments
test_that("downstream catchments correctly identified", {
  pa_sf <- catchments_sample[catchments_sample$CATCHNUM %in% c(192348, 199357, 191654),"CATCHNUM"]
  names(pa_sf)[1] <- "PA"
  
  expect_equal(
    as.data.frame(get_downstream_catchments(pa_sf = pa_sf, pa_id = "PA", catchments_sf = catchments_sample)),
    long_to_wide(data.frame(pas=c(191654, rep(192348, 2), rep(199357, 13)), catchnums=c(191337, 191450, 191396, 188154, 191960, 191982, 199251, 199255, 199703, 199708, 199662, 199672, 199658, 188071, 188051, 199392)),
                col_names = "pas", values_col = "catchnums")
  )
})

test_that("no downstream catchments", {
  pa_sf <- catchments_sample[catchments_sample$CATCHNUM %in% c(199392, 191396, 191337),"CATCHNUM"]
  names(pa_sf)[1] <- "PA"
  pa_sf$PA <- c("PA1", "PA2", "PA3")
  
  # expect an empty tibble with correct colname
  expect_equal(
    as.data.frame(get_downstream_catchments(pa_sf = pa_sf, pa_id = "PA", catchments_sf = catchments_sample)),
    data.frame('PA1' = as.integer(), 'PA2' = as.integer(), 'PA3' = as.integer())
  )
})