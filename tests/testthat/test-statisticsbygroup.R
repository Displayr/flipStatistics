context("Statistics by group")

test_that("Mean by group",
{
    df <- data.frame(x = c(100, 1:10, NA), y = 7)
    expect_equal(Mean(df, c(NA, 1:10, 100)), c(x = 7, y = 7))
    df <- rbind(df, data.frame(x = c(100, 1:10, NA), y = 8))
    z <- MeanByGroup(df, rep(LETTERS[1:2], c(12,12)), rep(c(NA, 1:10, 100), 2))
    expect_equal(as.numeric(z), c(7,7,7,8))
})

test_that("sum of squares by group",
          {
             df <- data.frame(x = c(-1, 0, NA, 1), y = c(NA, -1, NA, NA))
             zz <- rbind(A = SumOfSquares(df), B = SumOfSquares(df))
             df2 <- rbind(df, df)
             z <- SumOfSquaresByGroup(df2, rep(c("A", "B"), c(4,4)))
             expect_equal(z, zz)

             z <- SumOfSquaresByGroup(df2, rep(c("A", "B"), c(4,4)), weights = rep(10, 8))
             expect_equal(z, zz * 10)
          })


test_that("StatisticByGroup with a group with a single observation",
{
    ## See also @examples for StatisticsByGroup
    ## three observations/pairs of two identical variables
    dat <- matrix(0:2, 3, 2)

    groups <- c(1, 2, 3)
    expected.out <- dat
    expect_equal(unname(StatisticsByGroup(dat, groups)), expected.out)

    groups <- c(1, 1, 2)
    expected.out <- matrix(c(.5, 2), 2, 2)  # aggregate(dat, list(groups), Mean)
    expect_equal(unname(StatisticsByGroup(dat, groups)), expected.out)

    expected.out <- matrix(c(1.0, 2.0), 2, 2, dimnames = list(1:2, NULL))
    colSumsW <- function(x, weights = NULL) colSums(x)
    expect_equal(StatisticsByGroup(dat, groups, FUN = colSumsW), expected.out)
})
