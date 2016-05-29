#' StandardDeviation
#'
#' Computes the weighted variance of one or more variables
#'
#' @param x A \code{\link{data.frame}} or  \code{\link{matrix}}.
#' @param weights The sampling or replication weights.
#' @param method The way that the weights are addressed in the computation. Currently only "SPSS" is supported.
#' @export

StandardDeviation <- function(x, weights = NULL, method = "SPSS")
{
    sqrt(Variance(x, weights, method))
}
