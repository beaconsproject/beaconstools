library(beaconstools)

# gen_targets
test_that("targets are as expected with 5000 reserve size", {
  expect_snapshot(gen_targets(ref_poly, led_sample, 5000))
})

test_that("targets are as expected with 10000 reserve size", {
  expect_snapshot(gen_targets(ref_poly, led_sample, 10000))
})

test_that("targets are as expected with class_vals", {
  expect_snapshot(gen_targets(ref_poly, led_sample, 10000, c(1:5)))
})



# evaluate_targets_using_catchments
test_that("evaluation table is as expected", {
  target_table <- gen_targets(ref_poly, led_sample, 1600)
  expect_snapshot(evaluate_targets_using_catchments(catchments_sample, "led", benchmark_table_sample, target_table, colnames(benchmark_table_sample)))
})

test_that("unique catchments work in evaluation", {
  target_table <- gen_targets(ref_poly, led_sample, 1600)
  expect_equal(
    evaluate_targets_using_catchments(catchments_sample, "led", benchmark_table_sample, target_table, c("PB_0001"))$prop_target_met,
    evaluate_targets_using_catchments(catchments_sample, "led", benchmark_table_sample, target_table, c("PB_0001__PB_0001"))$prop_target_met
    )
})

test_that("benchmark order does not matter", {
  target_table <- gen_targets(ref_poly, led_sample, 1600)
  expect_equal(
    evaluate_targets_using_catchments(catchments_sample, "led", benchmark_table_sample, target_table, c("PB_0001__PB_0002"))$prop_target_met,
    evaluate_targets_using_catchments(catchments_sample, "led", benchmark_table_sample, target_table, c("PB_0002__PB_0001"))$prop_target_met
    )
})

test_that("default network_names works", {
  target_table <- gen_targets(ref_poly, led_sample, 1600)
  expect_equal(
    evaluate_targets_using_catchments(catchments_sample, "led", benchmark_table_sample, target_table, c("PB_0001","PB_0002","PB_0003")),
    evaluate_targets_using_catchments(catchments_sample, "led", benchmark_table_sample, target_table)
    )
})

# evaluate_targets_using_clip
test_that("evaluation table is as expected", {
  reserves <- catchments_to_networks(benchmark_table_sample, catchments_sample, c("PB_0001", "PB_0002"))
  target_table <- gen_targets(ref_poly, led_sample, 1600)
  expect_snapshot(evaluate_targets_using_clip(reserves, "network", led_sample, target_table))
})

test_that("benchmarks result matches evaluate_targets_using_catchments()", {
  reserves <- catchments_to_networks(benchmark_table_sample, catchments_sample, c("PB_0001", "PB_0002"))
  target_table <- gen_targets(ref_poly, led_sample, 1600)
  expect_equal(
    evaluate_targets_using_clip(reserves, "network", led_sample, target_table),
    evaluate_targets_using_catchments(catchments_sample, "led", benchmark_table_sample, target_table, c("PB_0001","PB_0002"))
    )
})

test_that("networks result matches evaluate_targets_using_catchments()", {
  reserves <- catchments_to_networks(benchmark_table_sample, catchments_sample, c("PB_0001__PB_0002"))
  target_table <- gen_targets(ref_poly, led_sample, 1600)
  expect_equal(
    evaluate_targets_using_clip(reserves, "network", led_sample, target_table),
    evaluate_targets_using_catchments(catchments_sample, "led", benchmark_table_sample, target_table, c("PB_0001__PB_0002"))
    )
})