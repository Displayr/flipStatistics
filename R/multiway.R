#' \code{Multiway}
#' @description Creates a crosstab by aggregating numeric data over
#'     factors.
#' @param rows A \code{\link{data.frame}} of variables to show in the
#'     rows of the table.
#' @param columns An optional \code{\link{data.frame}} of variables to
#'     show in the columns of the table.
#' @param numeric An optional numeric variable, which is used to
#'     compute the mean in the cells of the table.
#' @param numeric.statistics One or more of \code{"Mean"}, \code{"Minimum"},
#'     \code{"Maximum"}, \code{"Sum"}. The statistic to be computed
#'     when rows and columns have been specified. If \code{columns} is
#'     not supplied then can be a vector containing any of those four
#'     values. Ignored if \code{numeric} is not
#'     supplied.
#' @param numeric.statistic Deprecated in favour of
#'     \code{numeric.statistics}, provided for backwards
#'     compatibility. Ignored if \code{numeric} is not supplied or
#'     \code{numeric.statistics} is non-\code{NULL}.One of
#'     \code{"Mean"}, \code{"Minimum"}, \code{"Maximum"},
#'     \code{"Sum"}. The statistic to be computed when rows and
#'     columns have been specified. If \code{columns} is not supplied
#'     then can be a vector containing any of those four values
#'     (defaults to all four).
#' @param hide.empty.rows Hide rows containing no data.
#' @param hide.empty.columns Hide columns containing no data.
#' @param subset The sub-group to include in the analysis.
#' @param weights The sampling or replication weights.
#' @importFrom flipTransformations AsNumeric ProcessQVariables
#' @export
Multiway <- function(rows,
                     columns = NULL,
                     numeric = NULL,
                     numeric.statistics = NULL,
                     numeric.statistic = "Mean",
                     hide.empty.rows = FALSE,
                     hide.empty.columns = FALSE,
                     subset = NULL,
                     weights = NULL)
{
    has.columns <- !is.null(columns)
    ## Old Multiway Standard R items use numeric.statistic not numeric.statistics
    ## Preserve original behaviour by returning all four statistics when
    ## numeric.statistics is NULL and no column variables are supplied
    if (is.null(numeric.statistics))
    {
        if (has.columns)
            numeric.statistics <- numeric.statistic
        else
            numeric.statistics <- c("Count", "Mean", "Minimum", "Maximum", "Sum")
    }
    if (!is.data.frame(rows))
        rows <- data.frame(rows, stringsAsFactors = TRUE)

    rows <- ProcessQVariables(rows)
    columns <- ProcessQVariables(columns)
    numeric <- ProcessQVariables(numeric)

    n <- length(rows[[1]])
    # Cleaning subset.
    if (is.null(subset) || !is.null(subset) && length(subset) == 1)
        subset <- rep(TRUE, n)
    # Data preparation
    variable <- Interaction(rows, subset)
    values <- variable$values
    result <- variable$labels
    if (has.columns)
    {
        if (!is.data.frame(columns))
            columns <- data.frame(columns, stringsAsFactors = TRUE)
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
        if(!is.numeric(numeric))
            numeric <- AsNumeric(numeric, binary = FALSE)
    }
    # 1D
    if (!has.columns)
    {
        n.row.vars <- ncol(result)
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
            allowed.stats <- c("Count", "Mean", "Minimum", "Maximum", "Sum")
            if (!all(numeric.statistics %in% allowed.stats))
            {
                stat.list <- paste(allowed.stats, collapse = ", ")
                stop(sQuote("allowed.stats"), " must be one or more of: ", stat.list, ".")
            }
            m <- matrix(NA, nrow(result), ncol = length(allowed.stats) - 1)
            colnames(m) <- paste0(label, "\n", allowed.stats[-1])
            m[non.empty.rows, ] <- cbind(mean, min, max, sum)
            keep.idx <- c(seq_len(n.row.vars),
                          match(numeric.statistics, allowed.stats) + n.row.vars)
            result <- cbind(result, m)[, keep.idx]
        }
    }
    else # Crosstabs
    {
        numeric.statistic <- numeric.statistics[1L]
        n.rows <- nrow(result)
        n.columns <- nrow(columns$labels)
        n.p <- n.rows * n.columns
        if (n.p > 1e7)
            stop("The size of the multiway table is too large because there are too many different variables wth too many different levels. This may be fixed by removing numeric variables with a wide range of values.")
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
#' @param subset The sub-group to include in the analysis.
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
    labels <- data.frame(labels, stringsAsFactors = TRUE)
    names(labels) <- Labels(data)
    list(labels = labels, values = values)
}
