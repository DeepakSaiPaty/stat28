library(testthat)

cat("vec1: ")
test_that("q31",
expect_equal(vec1, c(20, 18, 16, 14, 12, 10, 8, 6, 4, 2, 0)))

cat("\nvec2: ")
test_that("q32",
expect_equal(vec2, c(2, 2, 2.666667, 4, 6.4, 10.666667, 18.285714, 32, 56.888889, 102.4), tolerance=0.001))

cat("\nvec3: ")
test_that("q33",
expect_equal(vec3, c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE)))

cat("\nvec4: ")
test_that("q34",
expect_equal(vec4, c(2, 4, 5, 2, 4, 5, 2, 4, 5, 2, 4, 5, 2, 4, 5, 2, 4, 5, 2, 4, 5, 2, 4, 5, 2, 4, 5, 2, 4, 5, 2)))

cat("\nvec5: ")
test_that("q35",
expect_equal(vec5, c(1,  1,  1,  1,  1,  2,  2,  2,  2,  2,
  3,  3,  3,  3,  3,  4,  4,  4,  4,  4,  5,  5,  5,  5,  5,
    6,  6,  6,  6,  6,  7,  7,  7,  7,  7,  8,  8,  8,  8,  8,
      9,  9,  9,  9,  9, 10, 10, 10, 10, 10)))


