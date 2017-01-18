library(testthat)

test_that("q7", {
expect_equal(sum(proportion.table[1, ]), 1, tolerance = 0.01)
expect_equal(sum(proportion.table[2, ]), 1, tolerance = 0.01)
expect_equal(sum(proportion.table[1, 1]), 0.612778316, tolerance = 0.01)
})
