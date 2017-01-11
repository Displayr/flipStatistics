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
