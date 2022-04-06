test_that("benchmark grouping works", {
  reserves <- dissolve_catchments_from_table(
    catchments_sample, 
    benchmark_table_sample,
    "network")
  
  expect_equal(group_benchmarks_using_grid(reserves, 10000),
               c(1,2,3)
               )
})

test_that("benchmark grouping works with bigger grid", {
  reserves <- dissolve_catchments_from_table(
    catchments_sample, 
    benchmark_table_sample,
    "network")
  
  expect_equal(group_benchmarks_using_grid(reserves, 100000),
               c(1,1,1)
               )
})


test_that("network grouping works", {
  reserves <- dissolve_catchments_from_table(
   catchments_sample, 
   benchmark_table_sample,
   "network")
  network_names <- gen_network_names(reserves$network, 2)
  networks_sf <- benchmarks_to_networks(reserves, network_names)
  
  expect_equal(group_networks_using_grid(networks_sf$network, reserves, "network", 10000),
               c(1,2,3)
               )
})

test_that("network grouping works with bigger grid", {
  reserves <- dissolve_catchments_from_table(
   catchments_sample, 
   benchmark_table_sample,
   "network")
  network_names <- gen_network_names(reserves$network, 2)
  networks_sf <- benchmarks_to_networks(reserves, network_names)
  
  expect_equal(group_networks_using_grid(networks_sf$network, reserves, "network", 100000),
               c(1,1,1)
               )
})