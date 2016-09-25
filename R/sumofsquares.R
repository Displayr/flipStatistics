#' SumOfSquares
#'
#' Computes the weighted sum of squares of one or more variables
#'
#' @param x A \code{\link{data.frame}} or  \code{\link{matrix}}.
#' @param weights The sampling or replication weights.
#' @export
SumOfSquares <- function(x, weights = NULL)
{
    if (!is.matrix(x))
        x <- as.matrix(x)
    if (is.null(weights))
    {
        sample.variance <- apply(x, 2, FUN = stats::var, na.rm = TRUE)
        ns <- apply(!is.na(x), 2, sum)
        ss <- sample.variance * (ns - 1)
        ss[ns == 1] <- 0
        return(ss)
    }
    if (nrow(x) != length(weights))
        stop("The number of rows in 'x' does not match the length 'weights and ")
    Ws <- matrix(weights, nrow(x), ncol(x))
    Ws[is.na(x)] <- NA
    sum.W <- apply(Ws, 2, sum, na.rm = TRUE)
    xbar <- Mean(x, weights)
    xxw <- sweep(x * x, 1, weights, "*")
    sum.xxw <- apply(xxw, 2, sum, na.rm = TRUE)
    (sum.xxw - sum.W * xbar * xbar)
}


#' SumOfSquares
#'
#' Computes the weighted sum of squares of one or more variables
#'
#' @param x A \code{\link{data.frame}} or  \code{\link{matrix}}.
#' @param group A variable indicating group membership. Either a factor or coerced to a factor.
#' @param weights The sampling or replication weights.
#' @export
SumOfSquaresByGroup <- function(x, group, weights = NULL)
{
    StatisticsByGroup(x, group, weights, SumOfSquares)
}
