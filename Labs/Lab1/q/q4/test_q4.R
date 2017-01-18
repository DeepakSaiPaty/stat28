library(testthat)

cat("a: ")
test_that("a", {
expect_true(all(df.a$Gender == "F"))
expect_true(nrow(df.a) == 4)
expect_true(ncol(df.a) == 6)
})

cat("\nb: ")
test_that("b", {
expect_true(all(df.b$Followers > 5000000))
expect_true(nrow(df.b) == 3)
expect_true(ncol(df.a) == 6)
})

cat("\nc: ")
test_that("c", {
expect_true(no.friends == 142)
})

cat("\nd: ")
test_that("d", {
expect_true(all(top3$Followers > 6000000))
expect_true(nrow(df.b) == 3)
})


