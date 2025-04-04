context("mean")

test_that("Mean is correct.", {
    # Small sample.
    expect_that(Mean(1:10), equals(5.5))
    expect_that(Mean(1:10, 1:10), equals(7))
    expect_that(Mean(c(100, 1:10, NA), c(NA, 1:10, 100)), equals(7))
    expect_that(Mean(cbind(1:10,1:10), 1:10)[2], equals(7))
})

test_that("Sum and Mean consistent with verbs", {
    library(verbs)
    variable.Nominal <- sample(c(NA, 1:5), 100, replace = TRUE) |>
        factor(levels = 1:5, labels = paste0(c(10, 20, 30, 40, 50), " to ", c(19, 29, 39, 49, 59))) |>
        structure(
            question = "Age",
            label = "Age",
            questiontype = "PickOne",
            dataset = "data.csv",
            name = "d1",
            codeframe = c(
                "10 to 19" = 15,
                "20 to 29" = 25,
                "30 to 39" = 35,
                "40 to 49" = 45,
                "50 to 59" = 55
            ),
            values = c(
                "10 to 19" = 15,
                "20 to 29" = 25,
                "30 to 39" = 35,
                "40 to 49" = 45,
                "50 to 59" = 55
            ),
            sourcevalues = c(
                "10 to 19" = 15,
                "20 to 29" = 25,
                "30 to 39" = 35,
                "40 to 49" = 45,
                "50 to 59" = 55
            )
        )
    variable.Numeric <- AsNumeric(variable.Nominal, binary = FALSE)
    attr(variable.Numeric, "questiontype") <- "Number"
    weights <- runif(length(variable.Numeric))
    expect_equal(Average(variable.Numeric, weights = weights),
                 Mean(variable.Numeric, weights = weights))
    variable.Binary <- sample(c(NA, 0, 1), 100, replace = TRUE) |>
        structure(question = "Coca-Cola", label = "Coca-Cola", questiontype = "binary")
    subset.binary <- !is.na(variable.Binary)
    data.frame(variable.Binary, variable.Nominal) |>
        AverageEachColumn(
            subset = subset.binary, weights = weights,
            remove.missing = FALSE
        ) |>
        expect_equal(
            c(
                "Coca-Cola" = flipStatistics::Mean(
                    variable.Binary[subset.binary],
                    weights = weights[subset.binary]
                ),
                "Age" = NA
            )
        )
    subset.missing.out <- !is.na(variable.Nominal)
    data.frame(variable.Binary, variable.Nominal) |>
        AverageEachColumn(subset = subset.missing.out, remove.missing = FALSE) |>
        expect_equal(
            c(
                "Coca-Cola" = NA,
                "Age" = Mean(flipTransformations::AsNumeric(variable.Nominal, binary = FALSE)[subset.missing.out])
            )
        )
    subset.missing.out <- !is.na(variable.Numeric)
    df.input <- data.frame(Age = variable.Numeric, Age = variable.Nominal, check.names = FALSE)
    AverageEachColumn(df.input, weights = weights, subset = subset.missing.out) |>
        expect_equal(
            c(
                Age = Mean(variable.Numeric, weights = weights),
                Age = Mean(
                    flipTransformations::AsNumeric(variable.Nominal, binary = FALSE),
                    weights = weights
                )
            )
        )
    df.input <- data.frame(Age = variable.Numeric, `Coca-Cola` = variable.Binary, check.names = FALSE)
    AverageEachColumn(df.input, weights = weights) |>
        expect_equal(
            c(
                Age = Mean(variable.Numeric, weights = weights),
                `Coca-Cola` = Mean(variable.Binary, weights = weights)
            )
        )
    AverageEachColumn(variable.Numeric, weights = weights) |>
        expect_equal(c(Age = Mean(variable.Numeric, weights = weights)))
})
