context("mean")

test_that("Mean is correct.", {
    # Small sample.
    expect_that(Mean(1:10), equals(5.5))
    expect_that(Mean(1:10, 1:10), equals(7))
    expect_that(Mean(c(100, 1:10, NA), c(NA, 1:10, 100)), equals(7))
    expect_that(Mean(cbind(1:10,1:10), 1:10)[2], equals(7))
})

