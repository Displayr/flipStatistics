#' Mean
#'
#' Computes the weighted mean of one or more variables. Missing values are automatically excluded.
#'
#' @param x A \code{\link{data.frame}} or  \code{\link{matrix}}.
#' @param weights The sampling or replication weights.
#' @export

Mean <- function(x, weights = NULL)
{
    if (!is.matrix(x))
        x <- as.matrix(x)
    if (is.null(weights))
        return(apply(x, 2, FUN = mean, na.rm = TRUE))
    Ws <- matrix(weights, nrow(x), ncol(x))
    Ws[is.na(x)] <- NA
    sum.W <- apply(Ws, 2, sum, na.rm = TRUE)
    xw <- sweep(x, 1, weights, "*")
    apply(xw, 2, sum, na.rm = TRUE) / sum.W
}

#' MeanByGroup
#'
#' @param x A \code{\link{data.frame}} or \code{\link{matrix}}.
#' @param group A variable indicating group membership. Either a factor or coerced to a factor.
#' @param weights The sampling or replication weights.
#' @export
MeanByGroup <- function(x, group, weights = NULL)
{
    StatisticsByGroup(x, group, weights, Mean)
}
