#' \code{Table}
#' @description Creates a crosstab by aggregating numeric data over factors.
#' @param formula A \code{formula} where the dependent variable is the variable to be aggregated over.
#' @param data A \code{data.frame}.
#' @param FUN the function to be applied: see \code{apply} for details.
#'
#' @importFrom stats terms xtabs aggregate
#' @export
Table <- function(formula, data, FUN = sum)
{
    has.outcome <- attr(terms(formula), "response") != 0
    if (!has.outcome)
    {
        if (!missing(FUN))
            stop("'FUN' can only be provided with a dependent variable.")
        return(xtabs(formula, data = data))
    }
    data <- aggregate(formula, data, FUN = FUN, drop = FALSE)
    remove.missing <- !is.na(FUN(numeric(0)))
    xtabs(formula, data = data, na.rm = remove.missing)
}
