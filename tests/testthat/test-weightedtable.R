context("WeightedTable")

data("cola", package="flipExampleData")
wg <- as.numeric(cola$LastResp)/4

test_that("WeightedTable", {
    tab0 <- table(cola$Q13, cola$Q14)
    tab1 <- WeightedTable(cola$Q13, cola$Q14)
    tab2 <- WeightedTable(cola$Q13, cola$Q14, weights=rep(0.1,327))
    tab3 <- WeightedTable(cola$Q13, cola$Q14, weights=wg)
    expect_equal(tab0, tab1)
    expect_equal(tab1/10, tab2)
    expect_equal(round(unname(tab3[,1])), c(423,635,224,353))
})

test_that("DS-5484", {
    wgt = 1:2
    x = c(NA, "A")
    expected = structure(c(A = 2),
                         .Dim = 1L,
                         .Dimnames = list(x = "A"),
                         class = "table")
    expect_equal(WeightedTable(x, weights = wgt), expected)

    expected = structure(c(2, 1),
                         .Dim = 2L,
                         .Dimnames = list(x = c("A", NA)),
                         class = "table")
    expect_equal(WeightedTable(x, weights = wgt, useNA = "ifany"), expected)
})

