#' \code{ExtractChartData}
#'
#' @description Returns data which is appropriate for charting
#'   The exact from of the data depends on the object
#' @param x Object containing the data. This is usually the output of a regression or machine learning output
#' @export
ExtractChartData <- function(x)
{
    UseMethod("ExtractChartData")
}

#' @export
ExtractChartData.default <- function(x)
{
    return(x)
}

#' @export
ExtractChartData.CorrelationMatrix <- function(x)
{
    res <- x$cor
    attr(res, "assigned.rownames") <- TRUE
    return(res)
}

#' @export
ExtractChartData.ts <- function(x)
{
    if (length(attr(x, "tsp")) == 3) # time-series object
    {
        ts.info <- attr(x, "tsp")
        ts.seq <- seq(from = ts.info[1], to = ts.info[2], by = 1/ts.info[3])
        if (length(dim(x)) < 2)
            names(x) <- ts.seq
        else
            rownames(x) <- ts.seq
        attr(x, "tsp") <- NULL     # delete attribute so we can do all matrix operations
        attr(x, "assigned.rownames") <- TRUE
    }
    return(x)
}
