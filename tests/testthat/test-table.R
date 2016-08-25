context("standardeviation")

test_that("StandardDeviation is correct.", {
    x <- data.frame(x = 1:10, y = 1:10)
    expect_equal(sum(Table(y ~ x, data = x, FUN = sum)), sum(1:10))
})

