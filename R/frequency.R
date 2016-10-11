#' \code{Frequency}
#'
#' @description Computes a frequency table, taking weights into account if applicable.
#' @param x The variable to be tablulated.
#' @param subset An optional subset.
#' @param weights an optional weight vector.
#' @export
Frequency <- function(x, subset = NULL, weights = NULL)
{
    if (!is.null(subset) && !is.null(weights))
        subset <- subset & weights > 0
    if (is.null(weights))
    {
        if (!is.null(subset))
            x <- subset(x, subset)
        return(table(x))
    }
    df <- data.frame(x, weights)
    if (!is.null(subset))
        df <- subset(df, subset)
    Table(weights ~ x, data = df)
}

