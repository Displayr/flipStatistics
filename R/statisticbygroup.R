#' StatisticsByGroup
#'
#' @param x A \code{\link{data.frame}} or  \code{\link{matrix}}.
#' @param group A variable indicating group membership. Either a factor or coerced to a factor.
#' @param weights The sampling or replication weights.
#' @param FUN A function that can handle a weight argument.
#' @export
StatisticsByGroup <- function(x, group, weights = NULL, FUN = Mean)
{
    if (!is.matrix(x) & !is.data.frame(x))
        x <- as.matrix(x)
    if (!is.factor(group))
        group <- factor(group)
    if (nrow(x) != length(group))
        stop("The number of rows in 'x' and 'group' length are not equivalent.")
    n.levels <- nlevels(group)
    levs <- levels(group)
    k <- ncol(x)
    result <- matrix(0, n.levels, k, dimnames = list(levs, names(x)))
    for (i in 1:n.levels)
    {
        lev <- levs[i] == group
        result[i, ] <- FUN(x[lev, , drop = FALSE], if (is.null(weights)) NULL else weights[lev])
    }
    result
}
