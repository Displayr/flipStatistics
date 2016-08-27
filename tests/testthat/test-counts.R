context("counts")

test_that("counts", {
    x <- data.frame(x = 1:10, y = 1:10)
    expect_equal(sum(WeightedCounts(x$x, x$y)), sum(1:10))
})
