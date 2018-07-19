#' @importFrom flipFormat ExtractChartData
#' @export
flipFormat::ExtractChartData

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
