
# append reserve
test_that("reserve is added as expected", {
  benchmarks <- dissolve_catchments_from_table(catchments_sample, benchmark_table_sample, "network", dissolve_list = c("PB_0001", "PB_0002"))
  pa_1 <- data.frame(lon = c(-85, -82.5, -83), lat = c(51, 51.5, 50.5)) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    dplyr::summarise(geometry = st_combine(geometry)) %>%
    sf::st_cast("POLYGON") %>%
    sf::st_transform(st_crs(benchmarks)) %>%
    dplyr::mutate(pa_name="a protected area", ha=9999, class="provincial")
  expect_snapshot(append_reserve(benchmarks, pa_1, "PA_1"))
})

test_that("reserve is dissolved correctly", {
  pa_1 <- dissolve_catchments_from_table(catchments_sample, benchmark_table_sample, "network", dissolve_list = c("PB_0003", "PB_0002"))
  benchmarks <- dissolve_catchments_from_table(catchments_sample, benchmark_table_sample, "network", dissolve_list = c("PB_0001"))
  expect_snapshot(append_reserve(benchmarks, pa_1, "PA_1"))
})

test_that("network error catches on name", {
  pa_1 <- dissolve_catchments_from_table(catchments_sample, benchmark_table_sample, "network", dissolve_list = c("PB_0003", "PB_0002"))
  benchmarks <- dissolve_catchments_from_table(catchments_sample, benchmark_table_sample, "network", dissolve_list = c("PB_0001"))
  names(benchmarks) <- c("networks", "geometry")
  expect_error(
    append_reserve(benchmarks, pa_1, "PA_1"),
    "network"
  )
})

test_that("network is converted to character", {
  pa_1 <- dissolve_catchments_from_table(catchments_sample, benchmark_table_sample, "network", dissolve_list = c("PB_0003", "PB_0002"))
  benchmarks <- dissolve_catchments_from_table(catchments_sample, benchmark_table_sample, "network", dissolve_list = c("PB_0001"))
  benchmarks$network <- as.factor(benchmarks$network)
  expect_equal(
    !is.character(benchmarks$network),
    is.character(append_reserve(benchmarks, pa_1, "PA_1")$network)
  )
})


# benchmarks_to_networks
test_that("network error catches", {
  benchmarks <- dissolve_catchments_from_table(catchments_sample, benchmark_table_sample, "network", dissolve_list = colnames(benchmark_table_sample))
  names(benchmarks) <- c("networks", "geometry")
  expect_error(
    benchmarks_to_networks(benchmarks, "PB_0001__PB_0002"),
    "network"
  )
})

test_that("geometry error catches", {
  benchmarks <- dissolve_catchments_from_table(catchments_sample, benchmark_table_sample, "network", dissolve_list = colnames(benchmark_table_sample))
  names(benchmarks) <- c("network", "geom")
  expect_error(
    benchmarks_to_networks(benchmarks, "PB_0001__PB_0002"),
    "geometry"
  )
})

test_that("benchmarks name error catches", {
  benchmarks <- dissolve_catchments_from_table(catchments_sample, benchmark_table_sample, "network", dissolve_list = colnames(benchmark_table_sample))
  expect_error(
    benchmarks_to_networks(benchmarks, "PB_0001__PB_0004"),
    "PB_0004"
  )
})

test_that("networks made as expected", {
  benchmarks <- dissolve_catchments_from_table(catchments_sample, benchmark_table_sample, "network", dissolve_list = colnames(benchmark_table_sample))
  expect_snapshot(benchmarks_to_networks(benchmarks, c("PB_0001__PB_0002", "PB_0001__PB_0003")))
})

test_that("benchmarks_to_networks results matches dissolve_catchments_from_table", {
  # geometries are different in the following object even though they describe the exact same area. 
  # I assume the list of points in the geometry value is different depending on the source polygons that were st_unioned (i.e. catchments vs benchmarks)
  # instead test the areas
  benchmarks <- dissolve_catchments_from_table(catchments_sample, benchmark_table_sample, "network", dissolve_list = colnames(benchmark_table_sample))
  expect_equal(
    sf::st_area(dissolve_catchments_from_table(catchments_sample, benchmark_table_sample, "network", dissolve_list = c("PB_0001__PB_0002", "PB_0001__PB_0003"))$geometry),
    sf::st_area(benchmarks_to_networks(benchmarks, c("PB_0001__PB_0002", "PB_0001__PB_0003"))$geometry)
  )
})


# list_overlapping_benchmarks
test_that("network error catches", {
  benchmarks <- dissolve_catchments_from_table(catchments_sample, benchmark_table_sample, "network", dissolve_list = colnames(benchmark_table_sample))
  names(benchmarks) <- c("networks", "geometry")
  expect_error(
    list_overlapping_benchmarks(benchmarks),
    "network"
  )
})

test_that("correct overlaps are returned", {
  benchmarks <- dissolve_catchments_from_table(catchments_sample, benchmark_table_sample, "network", dissolve_list = colnames(benchmark_table_sample))
  expect_equal(
    list_overlapping_benchmarks(benchmarks),
    c("PB_0001__PB_0003", "PB_0002__PB_0003")
  )
})

# dissolve catchments from table
# upstream application
test_that("upstream catchments dissolve correctly", {
  pa_sf <- catchments_sample[catchments_sample$CATCHNUM %in% c(199392, 191396, 191337),"CATCHNUM"]
  names(pa_sf)[1] <- "PA"
  upstream_table <- get_upstream_catchments(pa_sf = pa_sf, pa_id = "PA", catchments_sf = catchments_sample)
  
  catchments_sample_b <- catchments_sample
  catchments_sample_b$intactness <- c(rep(0.5, 40), rep(0.9, 47))
  
  expect_snapshot_output(
    as.data.frame(dissolve_catchments_from_table(catchments_sample_b, upstream_table, "PA", intactness_id = "intactness"))
  )
})

test_that("AWI warning", {
  pa_sf <- catchments_sample[catchments_sample$CATCHNUM %in% c(199392),"CATCHNUM"]
  names(pa_sf)[1] <- "PA"
  upstream_table <- get_upstream_catchments(pa_sf = pa_sf, pa_id = "PA", catchments_sf = catchments_sample)
  
  catchments_sample_b <- catchments_sample
  catchments_sample_b$intactness <- c(rep(0.5, 40), rep(0.9, 47))
  
  expect_warning(
    dissolve_catchments_from_table(catchments_sample_b, upstream_table, "PA", intactness_id = "intactnesss"),
    "Area-weighted intactness cannot be calculated"
  )
})

# dissolve benchmarks
test_that("benchmarks build as expected", {
  expect_snapshot(dissolve_catchments_from_table(catchments_sample, benchmark_table_sample, "network", dissolve_list = c("PB_0001", "PB_0002")))
})

test_that("networks build as expected", {
  expect_snapshot(dissolve_catchments_from_table(catchments_sample, benchmark_table_sample, "network", dissolve_list = c("PB_0001__PB_0002")))
})

test_that("CATCHNUM error catches", {
  expect_error(
    dissolve_catchments_from_table(catchments_sample[colnames(catchments_sample) != "CATCHNUM"], benchmark_table_sample, "network", dissolve_list = c("PB_0001__PB_0002")),
    "CATCHNUM"
  )
})

test_that("OID is removed", {
  
  benchmark_table_sample_oid <- benchmark_table_sample %>%
    dplyr::mutate(OID = 1:nrow(benchmark_table_sample))
  
  expect_equal(
    as.data.frame(dissolve_catchments_from_table(catchments_sample, benchmark_table_sample, "network")),
    as.data.frame(dissolve_catchments_from_table(catchments_sample, benchmark_table_sample_oid, "network"))
  )
})


# Extract catchments from table
test_that("extract works with one column",{
  expect_equal(extract_catchments_from_table(catchments_sample, benchmark_table_sample, "PB_0001", "network")$CATCHNUM,
               as.integer(benchmark_table_sample$PB_0001[!is.na(benchmark_table_sample$PB_0001)]))
})

test_that("extract works with multiple columns",{
  x <- unique(c(benchmark_table_sample$PB_0001, benchmark_table_sample$PB_0002, benchmark_table_sample$PB_0003))
  x <- x[!is.na(x)]
  
  expect_equal(sort(extract_catchments_from_table(catchments_sample, benchmark_table_sample, c("PB_0001", "PB_0002", "PB_0003"), "network")$CATCHNUM),
               sort(as.integer(x)))
})

test_that("names correct with single column",{
  expect_equal(extract_catchments_from_table(catchments_sample, benchmark_table_sample, "PB_0001", "network")$network[1],
               "PB_0001")
})

test_that("names correct with multiple columns",{
  expect_equal(extract_catchments_from_table(catchments_sample, benchmark_table_sample, c("PB_0001", "PB_0002"), "network")$network[1],
               "PB_0001__PB_0002")
})