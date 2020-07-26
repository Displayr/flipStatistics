context("Table")

test_that("Table", {
    x <- data.frame(x = 1:10, y = 1:10)
    expect_equal(sum(Table(y ~ x, data = x)), sum(1:10))

    df <- data.frame(val = 1:10,
            grp = factor(rep(letters[1:3], length = 10), levels = letters[1:4]))

    # should 'd' be NaN or 0?
    #expect_equal(as.vector(Table(val~grp, data = df, FUN = mean)),
    #        c(a = 5.5, b = 5, c = 6, d = NA), check.attributes = FALSE)

    expect_equal(as.vector(Table(val~grp, data = df, FUN = sum)),
            c(a = 22, b = 15, c = 18, d = 0), check.attributes = FALSE)
})

