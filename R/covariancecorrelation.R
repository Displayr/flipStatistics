# Compute a matrix containing the pairwise weighted correlations between each column in
# 'data'.
weightedPartialCovarianceMatrix <- function(data, weights, correlation = FALSE)
{
    num.cols <- ncol(data)
    output.matrix <- matrix(1, nrow = num.cols, ncol = num.cols,
        dimnames = list(colnames(data), colnames(data)))
    for (row in (1L + as.integer(correlation)):num.cols)
    {
        for (col in 1:row)
        {
            output.matrix[row, col] <- Correlation(data[, row], data[, col], weights, correlation)
            if (col != row)
                output.matrix[col, row] <- output.matrix[row, col]
        }
    }
    return(output.matrix)
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
#' @importFrom stats cor
#' @export
CovarianceAndCorrelationMatrix <- function(data,
    weights = NULL,
    pairwise = FALSE,
    use.correlation = TRUE)
{
    # Create the input correlation or convariance matrix
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
            weight = weights,
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

                dsgn <- svydesign(ids = ~1, weights = wgt, data = pair)
                v <- svyvar(pair, dsgn)
                correlations[i, j] <- v[1, 2] / sqrt(v[1, 1] * v[2, 2])
                correlations[j, i] <- correlations[i, j]
                t.stats[i, j] <- v[1, 2] / SE(v)[2]
                t.stats[j, i] <- t.stats[i, j]
                p.values[i, j] <- 2 * pt(-abs(t.stats[i, j]), sum(ind) - 2)
                p.values[j, i] <- p.values[i, j]
            }
            else
            {
                correlations[i, i] <- 1
                t.stats[i, i] <- Inf
                p.values[i, i] <- 0
            }
        }
    }
    list(cor = correlations, t = t.stats, p = p.values)
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
