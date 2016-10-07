context("frequency")

test_that("frequency",

          {
            x <- 1:11
            w <- c(1:10, NA)
            a <- Frequency(x)
            b <- table(x)
            expect_equal(as.numeric(a), as.numeric(b))
            expect_equal(names(a), names(b))
            a <- Frequency(x, weights = w)
            b <- (table(x) * w)[1:10]
            expect_equal(as.numeric(a), as.numeric(b))
            expect_equal(names(a), names(b))
            z <- Frequency(x, subset = x < 5 & x > 1, weights = w)
            q <- (table(x) * w)[2:4]
            expect_equal(as.numeric(a), as.numeric(b))
            expect_equal(names(a), names(b))
            a <- Frequency(x, subset = x < 5)
            b <- table(x)[1:4]
            expect_equal(as.numeric(a), as.numeric(b))
            expect_equal(names(a), names(b))
          })
