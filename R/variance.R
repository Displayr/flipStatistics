#' Variance
#'
#' Computes the weighted variance of one or more variables
#'
#' @param x A \code{\link{data.frame}} or  \code{\link{matrix}}.
#' @param weights The sampling or replication weights.
#' @param method The way that the weights are addressed in the computation. Currently only "SPSS" is supported.
#' @importFrom verbs SumColumns
#' @export

Variance <- function(x, weights = NULL, method = "SPSS")
{
    if (!is.matrix(x))
        x <- as.matrix(x)
    if (is.null(weights))
        return(apply(x, 2, FUN = stats::var, na.rm = TRUE))
    if (method != "SPSS")
        stop("Only SPSS supported in this function.")
    Ws <- matrix(weights, nrow(x), ncol(x))
    Ws[is.na(x)] <- NA
    sum.W <- SumColumns(Ws, remove.rows = NULL)
    # xw <- sweep(x, 1, weights, "*")
    xbar <- Mean(x, weights)
    xxw <- weights * x * x
    sum.xxw <- SumColumns(xxw, remove.rows = NULL)
    s2 <- (sum.xxw - sum.W * xbar * xbar) / (sum.W - 1)
    s2
}




