# Compute a matrix containing the pairwise weighted correlations between each column in
# 'X'.
weightedPartialCovarianceMatrix <- function(X, weights, correlation = FALSE)
{
    # Removing missing or inappropriate weights.
    valid <- !is.na(weights) & weights > 0
    weights <- weights[valid]
    X <- X[valid, ]
    # Remove entirely missing rows
    if (any(all.missing <- apply(X, 1, function(x) all(is.na(x)))))
    {
        not.all.missing <- !all.missing
        X <- X[not.all.missing, ]
        weights <- weights[not.all.missing]
    }
    # Use fast algorithm if no remaining missing values in X
    if (!anyNA(X))
        return(weightedCovarianceCorrelationMatrixCompleteCases(X, weights, correlation))
    # Remaining scenario has missing data spread throughout the data and needs
    # to be computed pairwise to correctly weight the data.
    num.cols <- ncol(X)
    output.matrix <- matrix(1, nrow = num.cols, ncol = num.cols,
                            dimnames = list(colnames(X), colnames(X)))
    for (row in (1L + as.integer(correlation)):num.cols)
        for (col in seq(row))
        {
            output.matrix[row, col] <- Correlation(X[, row], X[, col], weights, correlation)
            if (col != row)
                output.matrix[col, row] <- output.matrix[row, col]
        }
    output.matrix
}

#' \code{CovarianceAndCorrelationMatrix}
#'
#' @description Generate a covariance or correlation matrix from weighted or
#'   unweighted data using either the set of complete observations, or using
#'   pairwise-complete observations.
#'
#' @param data A data frame containing the input data.
#' @param weights A numeric vector containing the value of the weight for each
#'   row of \code{data}. If weights is NULL then this function is just a wrapper
#'   for the base functions \code{cov} and \code{cor}.
#' @param pairwise A logical value. If \code{TRUE} the correlations or
#'   covariances will be computed using the complete data for each pair of
#'   variables from \code{data}. If \code{FALSE} then cases with missing data
#'   will be excluded from the computation.
#' @param use.correlation A logical value specifying whether a correlation or
#'   covariance matrix should be returned.
#' @examples
#' my.data <- cbind(c(-0.9, 0.05, 0.1, 0.8), c(1, NaN, 0, -0.9))
#' my.weight <- c(1.2, 0.8, 0.8, 1.2)
#' CovarianceAndCorrelationMatrix(my.data, weights = my.weight, pairwise = TRUE)
#' @importFrom stats cor cov
#' @export
CovarianceAndCorrelationMatrix <- function(data,
    weights = NULL,
    pairwise = FALSE,
    use.correlation = TRUE)
{
    # Create the input correlation or covariance matrix
    if (is.null(weights))
    {   # Unweighted options
        use.string <- if (pairwise) "pairwise.complete.obs" else "complete.obs"
        input.matrix <- if (use.correlation) cor(data, use = use.string) else cov(data, use = use.string)
    }
    else
    {
        if (!pairwise)
        {
            complete.obs <- !is.na(rowSums(data)) & weights > 0
            data <- data[complete.obs, ]
            weights <- weights[complete.obs]
        }
        # Handles all cases
        input.matrix <- weightedPartialCovarianceMatrix(data,
            weights = weights,
            correlation = use.correlation)
    }
    return(input.matrix)
}

#' \code{Correlation}
#'
#' @description Computes the correlation between two vectors, using weights if provided.
#' @param x A numeric vector.
#' @param y A numeric vector.
#' @param weights A numeric vector containing the value of the weight for each
#'   row of \code{data}. If weights is NULL then this function is just a wrapper
#'   for the base functions \code{cov} and \code{cor}.
#' @param correlation If \code{FALSE}, returns the covariance.
#' @examples
#' x <- 1:10
#' y <- c(2, 2:10)
#' w <- rep(1, 10)
#' Correlation(x, y, w)
#' @export
Correlation <- function(x, y, weights = NULL, correlation = TRUE)
{
    if (is.null(weights))
        return(if (correlation) cor(x, y,  use = "complete.obs") else cov(x, y, use = "complete.obs"))
    # Removing missing values.
    valid <- !is.na(x) & !is.na(y) & !is.na(weights) & weights > 0
    weights <- weights[valid]
    x <- x[valid]
    y <- y[valid]

    Wx <- weights * x
    Wy <- weights * y
    Wxy <- Wx * y
    Wxx <- Wx * x
    Wyy <- Wy * y

    SumX <- sum(Wx)
    SumY <- sum(Wy)
    Sumxy <- sum(Wxy)
    SsX <- sum(Wxx)
    SsY <- sum(Wyy)

    # Calculate the Population for the
    # weights for the pairwise complete observations
    NWeighted <- sum(weights)

    # Covariance
    Value <- Sumxy - SumX * SumY / NWeighted
    if (!correlation)
        return(Value / (NWeighted - 1))
    # Correlation.
    SdX <- sqrt(SsX - SumX * SumX / NWeighted)
    SdY <- sqrt(SsY - SumY * SumY / NWeighted)
    return(Value / (SdX * SdY))
}

#' \code{CorrelationsWithSignificance}
#'
#' @description Computes correlations with significance from zero.
#' @param data A numeric data frame
#' @param weights A vector of weights
#' @param spearman Whether to compute Spearman correlations instead of Pearson correlations.
#' @return A list with four elements \itemize{
#' \item \code{correlation} A numeric correlation matrix of (possibly weighted) correlations computed on the input data.frame
#' \item \code{standard.errors} A numeric matrix with the standard errors of the computed correlations above
#' \item \code{statistics} A numeric matrix of the t-stastiscs computed based on the correlation and standard errors above
#' \item \code{p.values} A numeric matrix of p-values computed based on the t-statistics and df computed from the
#'  number of values used in the correlation calculation
#' }
#' @importFrom survey svydesign svyvar SE
#' @importFrom stats pt
#' @export
CorrelationsWithSignificance <- function(data, weights, spearman = FALSE)
{
    n <- ncol(data)
    mtrx <- matrix(NA, n, n)
    colnames(mtrx) <- names(data)
    rownames(mtrx) <- names(data)
    correlations <- mtrx
    t.stats <- mtrx
    p.values <- mtrx
    std.errs <- mtrx
    not.na.weights <- !is.na(weights)
    for (i in 1:n)
    {
        for (j in i:n)
        {
            if (i != j)
            {
                ind <- !is.na(data[, i]) & !is.na(data[, j]) & not.na.weights
                wgt <- weights[ind]
                pair <- if (spearman)
                    data.frame(SpearmanRanks(data[ind, i], wgt), SpearmanRanks(data[ind, j], wgt))
                else
                    data[ind, c(i, j)]

                if (nrow(pair) <= 1)
                {
                    correlations[i, j] <- NaN
                    t.stats[i, j] <- NA
                    p.values[i, j] <- NA
                    std.errs[i, j] <- NA
                }
                else
                {
                    dsgn <- svydesign(ids = ~1, weights = wgt, data = pair)
                    v <- svyvar(pair, dsgn)
                    tmp.val <- sqrt(v[1, 1] * v[2, 2])
                    correlations[i, j] <- v[1, 2] / tmp.val
                    tmp.cov.se <- SE(v)[2]
                    t.stats[i, j] <- v[1, 2] / tmp.cov.se
                    std.errs[i, j] <- tmp.cov.se / tmp.val
                    p.values[i, j] <- 2 * suppressWarnings(pt(-abs(t.stats[i, j]), sum(ind) - 2))
                }
                correlations[j, i] <- correlations[i, j]
                t.stats[j, i] <- t.stats[i, j]
                std.errs[j, i] <- std.errs[i, j]
                p.values[j, i] <- p.values[i, j]
            }
            else
            {
                correlations[i, i] <- 1
                t.stats[i, i] <- Inf
                p.values[i, i] <- 0
                std.errs[i, i] <- 0
            }
        }
    }
    list(cor = correlations, t = t.stats, p = p.values, standard.errors = std.errs)
}

#' \code{SpearmanRanks}
#'
#' @description Computes ranks for computing Spearman correlation.
#' @param x A numeric vector.
#' @param weights A vector of weights. Must not have missing values.
#' @export
SpearmanRanks <- function(x, weights)
{
    unique.vals <- sort(unique(x))
    previous.rank <- 0
    results <- rep(NA, length(x))
    for (i in seq(unique.vals))
    {
        ind <- x == unique.vals[i]
        population <- sum(weights[ind])
        results[ind] <- previous.rank + (population + 1) / 2
        previous.rank <- previous.rank + population
    }
    results
}


#' \code{CorrelationMatrix}
#'
#' @description Produces a correlation matrix from columns of data.
#' @param input.data Either a \code{\link{data.frame}}, a \code{\link{list}} of
#' \code{\link{data.frame}}s and/or \code{\link{vector}}s, or a \code{\link{matrix}}.
#' @param use.names Whether to use names in place of labels.
#' @param ignore.columns A list of names of columns to ignore. When \code{input.data}
#'   is a \code{\link{matrix}}, rows are also ignored. Typically \code{c("NET", "Total", "SUM")}.
#' @param missing.data Treatment of missing data. Options are \code{"Use partial data"}, \code{"Error if missing data"},
#'   or \code{"Exclude cases with missing data"}.
#' @param spearman Boolean whether to compute Spearman's correlation instead of Pearson's correlation.
#' @param filter An optional logical vector specifying a subset of values to be used.
#' @param weights An optional vector of sampling weights.
#' @param show.cell.values Either \code{"Yes"}, \code{"No"} or \code{"Automatic"}. \code{"Automatic"} displays
#'   values if there are <= 10 rows in the matrix.
#' @param row.labels Either \code{"Yes"} or \code{"No"} indicating whether row labels should be displayed.
#' @param column.labels Either \code{"Yes"} or \code{"No"} indicating whether row labels should be displayed.
#' @param colors A vector of colors used to create the colorbar.
#'   If not specified it will default to \code{RdBu} from colorbrewer.
#' @param colors.min.value Lower bound of the colorbar
#' @param colors.max.value Upper bound of the colorbar
#' @param categorical.as.binary Whether to convert factors to dummy binary variables, or else their
#'   levels are converted to integers.
#' @param input.type Deprecated. Now automatically deduced from \code{input.data}.
#' @export
CorrelationMatrix <- function(input.data, use.names = FALSE, ignore.columns = "",
                              missing.data = "Use partial data", spearman = FALSE,
                              filter = NULL, weights = NULL, show.cell.values = "Automatic",
                              colors = NULL, colors.min.value = -1, colors.max.value = 1,
                              row.labels = "Yes", column.labels = "Yes", input.type = NULL,
                              categorical.as.binary = FALSE)
{
    UseMethod("CorrelationMatrix")
}

# Default method for CorrelationMatrix.
#' @importFrom flipTransformations AsDataFrame AsNumeric
#' @export
CorrelationMatrix.default <- function(input.data, use.names = FALSE, ignore.columns = "",
                                      missing.data = "Use partial data", spearman = FALSE,
                                      filter = NULL, weights = NULL, show.cell.values = "Automatic",
                                      colors = NULL, colors.min.value = -1, colors.max.value = 1,
                                      row.labels = "Yes", column.labels = "Yes", input.type = NULL,
                                      categorical.as.binary = FALSE)
{
    dat <- AsDataFrame(input.data, use.names, ignore.columns, categorical.as.binary)

    if (ncol(dat) < 2)
        stop("Two or more columns of data are required to compute a ",
             "correlation matrix.")

    wgt <- if (is.null(weights)) {
        rep(1, nrow(dat))
    } else
        weights
    if (length(wgt) != nrow(dat))
        stop("Input data and weights must be same length.")

    if (is.null(filter) || (length(filter) == 1 && filter == TRUE))
        filter <- rep(TRUE, nrow(dat))
    if (length(filter) != nrow(dat))
        stop("Input data and filter must be same length.")

    dat <- dat[filter, ]
    wgt <- wgt[filter]

    processed.data <- if (missing.data == "Error if missing data") {
            errorIfMissingDataFound(dat)
        } else if (missing.data == "Exclude cases with missing data") {
            removeCasesWithAnyNA(dat)
        } else if (missing.data == "Use partial data") {
            removeCasesWithAllNA(dat)
        } else
            stop("Missing data option not handled: ", missing.data)

    if (nrow(processed.data) == 0)
        stop("No data remains after applying any filter and treatment of missing data.")

    wgt <- wgt[row.names(dat) %in% rownames(processed.data)]

    result <- CorrelationsWithSignificance(processed.data, wgt, spearman)
    result$colors.min.value <- as.numeric(colors.min.value)
    result$colors.max.value <- as.numeric(colors.max.value)

    result$show.cell.values <- show.cell.values
    result$row.labels <- row.labels
    result$column.labels <- column.labels
    result$colors <- if (is.null(colors)) "RdBu" else colors

    class(result) <- "CorrelationMatrix"
    attr(result, "ChartData") <- ExtractChartData(result)
    return(result)
}


#' \code{RemoveCasesWithAnyNA}
#'
#' @description Remove rows which contain NA.
#' @param x The input dataframe.
removeCasesWithAnyNA <- function(x)
{
    x[apply(is.na(x), 1, sum) == 0, , drop = FALSE]
}


errorIfMissingDataFound <- function(data)
{
    if (any(is.na(data)))
        missingDataFail()
    data
}

missingDataFail <- function()
{
    stop("The data contains missing values. Change the 'missing' option to run the analysis.")
}


#' \code{RemoveCasesWithAllNA}
#'
#' @description Remove rows which are all NA.
#' @param x The input dataframe.
removeCasesWithAllNA <- function(x)
{
    x[apply(is.na(x), 1, sum) < ncol(x), , drop = FALSE]
}
#' \code{print.CorrelationMatrix}
#'
#' @param x An object of class \code{\link{CorrelationMatrix}}.
#' @param ... Other paramaters, not used.
#' @details Displays a correlation matrix as a heatmap.
#' @importFrom flipFormat FormatAsReal
#' @export
#' @method print CorrelationMatrix
print.CorrelationMatrix <- function(x, ...) {

    n <- ncol(x$cor)
    cellnote <- matrix("", n, n)
    t.stat <- matrix("", n, n)
    p.val <- matrix("", n, n)
    for (i in 1:n)
        for (j in 1:n)
        {
            cellnote[i, j] <- FormatAsReal(x$cor[i, j], decimals = 2)
            t.stat[i, j] <- FormatAsReal(x$t[i, j], decimals = 3)
            p.val[i, j] <- FormatAsReal(x$p[i, j], decimals = 3)
        }

    show.cellnote.in.cell <- (n <= 10 && x$show.cell.values != "No") || x$show.cell.values == "Yes"
    show.x.axes.labels <- x$column.labels == "Yes"
    show.y.axes.labels <- x$row.labels == "Yes"

    tooltip.info <- list("t-statistic" = t.stat,
                         "p-value" = p.val)
    x$cor[which(x$cor < x$colors.min.value)] <- NA
    x$cor[which(x$cor > x$colors.max.value)] <- NA
    correlation.matrix <- rhtmlHeatmap::Heatmap(x$cor, Rowv = NULL, Colv = NULL,
                                                cellnote = cellnote, colors = x$colors,
                                                show_cellnote_in_cell = show.cellnote.in.cell,
                                                xaxis_location = "bottom", yaxis_location = "left",
                                                lower_triangle = TRUE, cexRow = 0.79,
                                                xaxis_hidden = !show.x.axes.labels,
                                                yaxis_hidden = !show.y.axes.labels,
                                                color_range = c(x$colors.min.value, x$colors.max.value),
                                                extra_tooltip_info = tooltip.info)

    print(correlation.matrix)
}

#' Use a faster algorithm to compute the covariance or correlation when no missing data is present.
#' @param X A numeric matrix with no missing values to compute the covariance or correlation
#' @param weight A numeric vector of weights to use
#' @param correlation A logical indicator to compute correlation (\code{TRUE}) or covariance (\code{FALSE})
#' @noRd
weightedCovarianceCorrelationMatrixCompleteCases <- function(X, weights, correlation = TRUE)
{
    # Check if the weights are trivial and redundant
    if (all(weights == weights[1]))
    {
        # Center the matrix
        X <- t(X) - colMeans(X)
        # Either scale to have variance 1 for correlation or divide only by df for covariance
        if (correlation)
            X <- X/sqrt(rowSums(X^2))
        output.matrix <- tcrossprod(X)
        if (!correlation)
            output.matrix <- output.matrix/(nrow(X) - 1)
        # Add the labels
        colnames(output.matrix) <- rownames(output.matrix) <- rownames(X)
        return(output.matrix)
    }
    # From here the weights are not trivial and need to be used in the computation
    # Start by computing the weighted means and centering the matrix
    n.weights <- sum(weights)
    means <- colSums(weights * X)/n.weights
    X <- t(sqrt(weights) * (X - rep(means, each = nrow(X))))
    # Either scale or divide by df, equivalent to the above calculation without weights.
    if (correlation)
        X <- X/sqrt(rowSums(X^2))
    output.matrix <- tcrossprod(X)
    if (!correlation)
        output.matrix <- output.matrix/(n.weights - 1)
    # Add the labels
    rownames(output.matrix) <- colnames(output.matrix) <- rownames(X)
    output.matrix
}
