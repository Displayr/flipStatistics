context("sum of squares")

test_that("sum of squares",
          {
             df <- data.frame(x = c(-1, 0, 1), y = c(-1, NA, 1))
             expect_equal(SumOfSquares(df), c(x = 2, y = 2))
             df <- data.frame(x = c(-1, 0, 1), y = c(-1, NA, 1))
             expect_equal(SumOfSquares(df, weights = rep(10, 3)), c(x = 20, y = 20))
             df <- data.frame(x = c(-1, -1, -1), y = c(-1, NA, NA))
             expect_equal(SumOfSquares(df), c(x = 0, y = 0))
             expect_equal(SumOfSquares(df, weights = rep(10, 3)), c(x = 0, y = 0))
          })
