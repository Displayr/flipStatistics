#' \code{Multiway}
#' @description Creates a crosstab by aggregating numeric data over factors.
#' @param rows A \code{\link{data.frame}} of variables to show in the rows of the table.
#' @param columns An optional \code{\link{data.frame}} of variables to show in the columns of the table.
#' @param numeric An optional numeric variable, which is used to compute the mean in the cells of the table.
#' @param numeric.statistic One of \code{"Mean"}, \code{"Minimum"}, \code{"Maximum"}, \code{"Sum"}.The statistic to be computed when rows and columns have been specified. Ignored if
#' no columns have been specified.
#' @param hide.empty.rows Hide rows containing no data.
#' @param hide.empty.columns Hide columns containing no data.
#' @param subset The sub-group to include in the analsis.
#' @param weights The sampling or replication weights.
#' @importFrom flipTransformations AsNumeric
#' @export
Multiway <- function(rows,
                     columns = NULL,
                     numeric = NULL,
                     numeric.statistic = "Mean",
                     hide.empty.rows = FALSE,
                     hide.empty.columns = FALSE,
                     subset = NULL,
                     weights = NULL)
{
    if (!is.data.frame(rows))
        rows <- data.frame(rows)
    n <- length(rows[[1]])
    # Cleaning subset.
    if (is.null(subset) || !is.null(subset) && length(subset) == 1)
        subset <- rep(TRUE, n)
    # Data preparation
    variable <- Interaction(rows, subset)
    values <- variable$values
    result <- variable$labels
    if (has.columns <- !is.null(columns))
    {
        if (!is.data.frame(columns))
            columns <- data.frame(columns)
        columns <- Interaction(columns, subset)
        n.rows <- nrow(result)
        values <- values + (columns$values - 1) * n.rows
    }
    # 1D
    counts <- Frequency(values, subset, weights)
    # Numeric.
    if (has.numeric <- !is.null(numeric))
    {
        label <- Labels(numeric)
        numeric <- AsNumeric(numeric, binary = FALSE)
    }
    # 1D
    if (!has.columns)
    {
        result$Count <- 0
        if (!is.null(weights))
            names(result)[ncol(result)] <- "Weighted count"
        non.empty.rows <- as.integer(names(counts))
        result$Count[non.empty.rows] <- counts
        if (!is.null(numeric))
        {
            mean <- MeanByGroup(numeric, values, weights)
            min <- StatisticsByGroup(numeric, values, weights, FUN = Min)
            max <- StatisticsByGroup(numeric, values, weights, FUN = Max)
            sum <- mean * as.matrix(counts)
            m <- matrix(NA, nrow(result), ncol = 4)
            colnames(m) <- paste0(label, "\n", c("Mean", "Minimum", "Maximum", "Sum"))
            m[non.empty.rows, ] <- cbind(mean, min, max, sum)
            result <- cbind(result, m)
        }
    }
    else # Crosstabs
    {
        n.rows <- nrow(result)
        n.columns <- nrow(columns$labels)
        n.p <- n.rows * n.columns
        m <- matrix(if(has.numeric) NA else 0, n.rows, n.columns)
        column.labels <- apply(columns$labels, 1, paste, collapse = "\n")
        if (has.numeric)
        {
            column.labels <- paste0(column.labels, "\n", numeric.statistic, ": ", label)
            counts <- StatisticsByGroup(numeric, values, weights, FUN = switch(numeric.statistic,
                                                                               "Minimum" = Min,
                                                                               "Maximum" = Max,
                                                                               "Mean" = Mean,
                                                                               "Sum" = Sum))
            nms <- rownames(counts)
        }
        else
            nms <- names(counts)
        mtch <- match(nms, 1:n.p)
        colnames(m) <- column.labels
        m[mtch] <- counts
        if (hide.empty.columns | hide.empty.rows)
        {
            has.data <- if (has.numeric) !is.na(m) else m > 0
            if (hide.empty.columns)
            {
                empty.columns <- apply(has.data, 2, sum) == 0
                m <- m[, !empty.columns]
            }
            if (hide.empty.rows)
                non.empty.rows <- apply(has.data, 1, sum) > 0
        }
        result <- cbind(result, m)
    }
    if (hide.empty.rows)
        result <- result[non.empty.rows, ]
    result
}

#' Interaction
#'
#' @param data A \code{\link{data.frame}} of variables.
#' @importFrom flipFormat Labels
#' @importFrom flipTransformations Factor
#' @param subset The sub-group to include in the analsis.
#' @export
Interaction <- function(data, subset)
{
    n.variables <- ncol(data)
    n <- length(data[[1]])
    for (i in 1:n.variables)
        if (!is.factor(data[[i]]))
             data[[i]] <- Factor(data[[i]])
    # Computing values
    labels <- matrix("", prod(sapply(data, nlevels)), n.variables)
    prod.n.levels <- 1
    values <- rep(1, n)
    for (i in 1:n.variables)
    {
        var <- data[[i]]
        var[!subset] <- NA
        values <- values + (unclass(var) - 1) * prod.n.levels
        levs <- levels(var)
        n.levels <- length(levs)
        labels[, i] <- rep(levs, rep(prod.n.levels, n.levels))
        prod.n.levels <- prod.n.levels * n.levels
    }
    labels <- data.frame(labels)
    names(labels) <- Labels(data)
    list(labels = labels, values = values)
}
