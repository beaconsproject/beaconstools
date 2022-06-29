# sum_ranks
test_that("ranks are as expected", {
  tbl <- data.frame(col1 = c(1,2,3,4,5), col2 = c(0.4, 0.5, 0.2, 0.05, 0.9), col3 = c(10, 1, 8, 11, 1))
  
  expect_equal(
    sum_ranks(tbl, 'col1', 'col2'),
    c(4, 4, 7, 9, 6)
  )
})

test_that("test all NA error", {
  tbl <- data.frame(col1 = c(1,2,3,4,5), col2 = c(0.4, 0.5, 0.2, 0.05, 0.9), col3 = c(NA, NA, NA, NA, NA))
  
  expect_error(
    sum_ranks(tbl, c('col1', 'col3'), 'col2'),
    "All values in column"
  )
})

test_that("test some NA", {
  tbl <- data.frame(col1 = c(1,2,3,4,5), col2 = c(0.4, 0.5, 0.2, 0.05, 0.9), col3 = c(NA, NA, 1, NA, NA))
  
  expect_equal(
    sum_ranks(tbl, c('col1', 'col3'), 'col2'),
    c(7.5, 7.5, 8, 12.5, 9.5)
  )
})

test_that("test single column asc", {
  tbl <- data.frame(col1 = c(1,2,3,4,5), col2 = c(0.4, 0.5, 0.2, 0.05, 0.9), col3 = c(NA, NA, 1, NA, NA))
  
  expect_equal(
    sum_ranks(tbl, columns_asc = 'col1'),
    c(1,2,3,4,5)
  )
})

test_that("test single column desc", {
  tbl <- data.frame(col1 = c(1,2,3,4,5), col2 = c(0.4, 0.5, 0.2, 0.05, 0.9), col3 = c(NA, NA, 1, NA, NA))
  
  expect_equal(
    sum_ranks(tbl, columns_desc = 'col1'),
    c(5,4,3,2,1)
  )
})

test_that("test wrong column name", {
  tbl <- data.frame(col1 = c(1,2,3,4,5), col2 = c(0.4, 0.5, 0.2, 0.05, 0.9), col3 = c(NA, NA, 1, NA, NA))
  
  expect_error(
    sum_ranks(tbl, columns_desc = 'col4'),
    "Columns not in table"
  )
})
