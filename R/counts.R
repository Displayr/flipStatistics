#' #' \code{WeightedCounts} Computes a SVD with frequency weights.
#' #' @param x A numeric vector.
#' #' @param weights Frequency Weights.
#' #' @example
#' #' @export
#' WeightedCounts = function(x, weights){
#'   tapply(weights, x, FUN = "sum", na.rm = TRUE)
#' }

