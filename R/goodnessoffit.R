#' \code{GoodnessOfFitPlot}
#' @description A generic function used to produce plots illustrating the goodness-of-fit of
#' the model object.  The function invokes particular \code{\link{methods}}
#' which depend on the \code{\link{class}} of the first argument.
#'
#' Reports the goodness-of-fit of an object.
#' @param object An object for which a summary is desired.
#' @param ... Additional arguments affecting the goodness-of-fit displayed.
#' @param max.points The maximum numner of points to plot.
#' @export
GoodnessOfFitPlot <- function(object, max.points = 1000, ...) {

    UseMethod("GoodnessOfFitPlot")
}
