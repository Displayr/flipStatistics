#' StatisticsByGroup
#'
#' @param x A \code{\link{data.frame}} or  \code{\link{matrix}}.
#' @param group A variable indicating group membership. Either a factor or coerced to a factor.
#' @param weights The sampling or replication weights.
#' @param FUN A function.
#' @export
StatisticsByGroup <- function(x, group, weights = NULL, FUN = Mean)
{
    if (!is.factor(group))
        group <- factor(group)
    n.levels <- nlevels(group)
    levs <- levels(group)
    k <- ncol(x)
    result <- matrix(0, n.levels, k, dimnames = list(levs, names(x)))
    for (i in 1:n.levels)
    {
        lev <- levs[i] == group
        result[i, ] <- FUN(x[lev, ], if (is.null(weights)) NULL else weights[lev])
    }
    result
}
