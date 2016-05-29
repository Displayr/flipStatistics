context("standardeviation")

test_that("StandardDeviation is correct.", {
    # Small sample.
    expect_that(StandardDeviation(1:10), equals(StandardDeviation(1:10, rep(1, 10))))
    expect_that(StandardDeviation(1:10, 1:10), equals(StandardDeviation(rep(1:10, 1:10))))
    expect_that(StandardDeviation(1:10, 1:10), equals(StandardDeviation(c(100, 1:10, NA), c(NA, 1:10, 100))))
    expect_that(StandardDeviation(cbind(1:10, 1), 1:10)[1], equals(StandardDeviation(rep(1:10, 1:10))))
    expect_that(Mean(c(100, 1:10, NA), c(NA, 1:10, 100)), equals(7))
    expect_that(Mean(cbind(1:10,1:10), 1:10)[2], equals(7))
})

