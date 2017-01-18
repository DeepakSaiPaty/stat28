library(testthat)

test_that("q5", {
expect_equal(exercise.1, 0.2196956, tolerance = 0.001)
expect_equal(calc.norm(1, 2, 0), 0.2196956, tolerance = 0.001)
expect_equal(calc.norm(0, 1, 0), 0.3989423, tolerance = 0.001)
})

