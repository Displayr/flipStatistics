context("Table")

test_that("Table", {
    x <- data.frame(x = 1:10, y = 1:10)
    expect_equal(sum(Table(y ~ x, data = x)), sum(1:10))
})

