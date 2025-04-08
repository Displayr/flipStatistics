#' \code{WeightedCounts} Computes a SVD with frequency weights.
#' @param x A factor.
#' @param weights Frequency Weights.
#' @export
WeightedCounts = function(x, weights)
{
   Table(weights ~ x, data.frame(x = x, weights = weights), FUN = sum)
}
# Test comment
