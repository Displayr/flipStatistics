# Compute a matrix containing the pairwise weighted correlations between each column in
# 'data'.
weightedPartialCovarianceMatrix <- function(data, weight, correlation = FALSE)
{
    .weightedPartialCovariance <- function(numeric1, numeric2, input.weight, correlation = FALSE)
    {
        Sumxy <- 0.0
        SumY <- 0.0
        SumX <- 0.0
        SsX <- 0.0
        SsY <- 0.0


        # Vector Version

        Wx <- input.weight * numeric1
        Wy <- input.weight * numeric2
        Wxy <- Wx * numeric2
        Wxx <- Wx * numeric1
        Wyy <- Wy * numeric2

        complete.indicator <- !is.na(Wxy) & input.weight > 0

        SumX <- sum(Wx[complete.indicator])
        SumY <- sum(Wy[complete.indicator])
        Sumxy <- sum(Wxy[complete.indicator])
        SsX <- sum(Wxx[complete.indicator])
        SsY <- sum(Wyy[complete.indicator])

        # Calculate the Population for the
        # weights for the pairwise complete observations
        NWeighted <- sum(input.weight[complete.indicator])

        # Covariance
        Value <- Sumxy - SumX * SumY / NWeighted
        if (correlation)
        {
            SdX <- sqrt(SsX - SumX * SumX / NWeighted)
            SdY <- sqrt(SsY - SumY * SumY / NWeighted)
            denominator <- SdX * SdY
            return(Value / denominator)
        } else {
            return(Value / (NWeighted-1))
        }
    }

    num.cols <- ncol(data)
    output.matrix <- matrix(0, nrow = num.cols, ncol = num.cols,
        dimnames = list(colnames(data), colnames(data)))
    for (row in 1L:num.cols)
    {
        for (col in row:num.cols)
        {
            output.matrix[row, col] <- .weightedPartialCovariance(numeric1 = data[, row],
                numeric2 = data[, col],
                input.weight = weight,
                correlation = correlation)
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
#' @importFrom flipData ExcludeCasesWithAnyMissingData
#' @export
CovarianceAndCorrelationMatrix <- function(data,
    weights = NULL,
    pairwise = FALSE,
    use.correlation = TRUE)
{
    # Create the input correlation or convariance matrix
    if (is.null(weights))
    {
        # Unweighted options
        if (pairwise)
        {
            use.string <- "pairwise.complete.obs"
        }
        else
        {
            use.string <- "complete.obs"
        }

        if (use.correlation)
        {
            input.matrix <- stats::cor(data, use = use.string)
        }
        else
        {
            input.matrix <- stats::cov(data, use = use.string)
        }
    }
    else
    {
        if (!pairwise)
        {
            data <- ExcludeCasesWithAnyMissingData(data)
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
#' @examples
#' x <- 1:10
#' y <- c(2, 2:10)
#' w <- rep(1, 10)
#' Correlation(x, y, w)
#' @export
Correlation <- function(x, y, weights = NULL)
{
    CovarianceAndCorrelationMatrix(data.frame(x, y), weights, TRUE, TRUE)[2,1]
}


