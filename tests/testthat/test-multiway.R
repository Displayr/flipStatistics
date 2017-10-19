context("multiway")



data(colas, package = "flipExampleData")
attach(colas)
test_that("Frequency",
          {
              d0 <- factor(rep("Cat", 327))
              levels(d0) <- c("Cat", "Dog")
              f <- Multiway(data.frame(d1, d0,  d2, d3), hide.empty.rows = TRUE)
              expect_equal(sum(f$Count), sum(xtabs(~d1 + d2 + d3)))
              f <- Multiway(list(d1, d2, d3), hide.empty.rows = TRUE)
              expect_equal(sum(f$Count), sum(xtabs(~d1 + d2 + d3)))
              f1 <- Multiway(data.frame(d1, d2, d3), hide.empty.rows = FALSE)
              expect_equal(sum(f1$Count), sum(xtabs(~d1 + d2 + d3)))
              expect_true(nrow(f1) > nrow(f))
          })

test_that("Means",
          {
              cnt <- sum(xtabs(~d1 + d2 + d3))
              av <- sum(xtabs(unclass(d4) ~ d1 + d2 + d3) / xtabs(~d1 + d2 + d3), na.rm = TRUE)
              f <- suppressWarnings(Multiway(data.frame(d1, d2, d3), numeric = d4,  hide.empty.rows = TRUE))
              expect_equal(sum(f$Count), cnt)
              expect_equal(sum(f[, 5]), av)
              f1 <- suppressWarnings(Multiway(data.frame(d1, d2, d3), numeric = d4,  hide.empty.rows = FALSE))
              expect_equal(sum(f1$Count), cnt)
              expect_equal(sum(f1[, 5], na.rm = TRUE), av)
})

test_that("Crosstab",
          {
              set.seed(1)
              f <- Multiway(data.frame(d1, q8), data.frame(d4, q7), hide.empty.rows = TRUE)
              expect_equal(sum(f[,-1:-2]), sum(xtabs(~d1 + q8 + d4 + q7)))
              wgt <- runif(327)
              f <- Multiway(data.frame(d1, q8), data.frame(d4, q7), hide.empty.rows = TRUE, weights = wgt)
              expect_equal(sum(f[,-1:-2]), sum(xtabs(wgt~d1 + q8 + d4 + q7)))
              sb <- runif(327) > .5
              f <- Multiway(data.frame(d1, q8), data.frame(d4, q7), hide.empty.rows = TRUE, weights = wgt, subset = sb)
              expect_equal(sum(f[,-1:-2]), sum(xtabs(wgt~d1 + q8 + d4 + q7, subset = sb)))
              expect_error(Multiway(data.frame(d1), columns = data.frame(d2, d4, q1a, q1b, q2a, q2b, q2c)), "The size of the multiway.")
          })

test_that("Table of means",
          {
              set.seed(1)
              f <- suppressWarnings(Multiway(data.frame(d1, q8), numeric = d2,  data.frame(d4, q7), hide.empty.rows = TRUE, numeric.statistic = "Sum"))
              expect_equal(sum(f[,-1:-2], na.rm = TRUE), sum(unclass(d2[complete.cases(cbind(d1, q8, d4, q7, d2))])))
              wgt <- runif(327)
              f <- suppressWarnings(Multiway(data.frame(d1, q8), numeric = d2,  data.frame(d4, q7), hide.empty.rows = TRUE, numeric.statistic = "Sum", weights = wgt))
              expect_equal(sum(f[,-1:-2], na.rm = TRUE), sum((wgt*unclass(d2))[complete.cases(cbind(d1, q8, d4, q7, d2))]))
              sb <- runif(327) > .5
              f <- suppressWarnings(Multiway(data.frame(d1, q8), numeric = d2,  data.frame(d4, q7), hide.empty.rows = TRUE, numeric.statistic = "Sum", weights = wgt, subset = sb))
              expect_equal(sum(f[,-1:-2], na.rm = TRUE), sum((as.integer(sb) * wgt*unclass(d2))[complete.cases(cbind(d1, q8, d4, q7, d2))]))

f <- suppressWarnings(Multiway(data.frame(d1), numeric = d2,  data.frame(d4, q7, q8), hide.empty.rows = TRUE, numeric.statistic = "Sum"))
          })


detach(colas)
test_that("Data types for the numeric variable should be passed when reading in a numeric variable",
          {
              data(phone, package = "flipExampleData")
              attach(phone)
            Multiway(list(q2, q3), list(q4), q25)
              detach(phone)
    # if (length(formNumeric) == 1) NULL else formNumeric,
    # numeric.statistic = formStatistic,
    # hide.empty.rows = formHideRows,
    # hide.empty.columns = formHideColumns,
    # subset = QFilter,
    # weights = QPopulationWeight)
})



#

# id <- 1:327
# Multiway(data.frame(d1), numeric = d2,  columns = data.frame(d3, id), hide.empty.columns = FALSE, numeric.statistic = "Sum")
# detach(colas)
#
#
#
# Multiway(data.frame(d1), numeric = d2,  columns = data.frame(d4, q7, q8), hide.empty.columns = FALSE, numeric.statistic = "Sum")
#
#
# Multiway(data.frame(d1), numeric = d2,  columns = data.frame(d4, q7, q8), hide.empty.columns = FALSE, numeric.statistic = "Sum")
#


# Interaction(data.frame(d1, d2, d3))
#
#
#     a <- gl(2, 4, 8)
# b <- gl(2, 2, 8, labels = c("ctrl", "treat"))
# s <- gl(2, 1, 8, labels = c("M", "F"))
#
#
# a <- factor(rep(c("Male","Female")[c(1,1,1,1,2,2,2,2)]))
# b <- factor(rep(c("Young", "Old")[c(1,1,2,2,1,1,2,2)]))
# c <- factor(rep(c("Short","Tall")[c(1, 2, 1, 2, 1, 2, 1, 2)]))
# z <- runif(8)
#
# interaction(list(a, b, c))
#
#
#
# interaction(a, b, s, sep = ":")
# stopifnot(identical(a:s,
#                     interaction(a, s, sep = ":", lex.order = TRUE)),
#           identical(a:s:b,
#                     interaction(a, s, b, sep = ":", lex.order = TRUE)))
