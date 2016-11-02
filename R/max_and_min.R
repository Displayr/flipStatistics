#' Max
#'
#' Computes the weighted maximum of one or more variables. Missing values are automatically excluded.
#'
#' @param x A \code{\link{data.frame}} or  \code{\link{matrix}}.
#' @param weights The sampling or replication weights.
#' @details Computes the maximum of the observations with positive weights.
#' @export
Max <- function(x, weights = NULL)
{
    if (!is.matrix(x))
        x <- as.matrix(x)
    if (!is.null(weights))
        x <- x[weights == 0 | is.na(weights)]
    apply(x, 2, FUN = max, na.rm = TRUE)
}

#' Min
#'
#' Computes the weighted minimum of one or more variables. Missing values are automatically excluded.
#' @param x A \code{\link{data.frame}} or  \code{\link{matrix}}.
#' @param weights The sampling or replication weights.
#' @details Computes the mininum of the observations with positive weights.
#' @export
Min <- function(x, weights = NULL)
{
    if (!is.matrix(x))
        x <- as.matrix(x)
    if (!is.null(weights))
        x <- x[weights == 0 | is.na(weights)]
    apply(x, 2, FUN = min, na.rm = TRUE)
}

