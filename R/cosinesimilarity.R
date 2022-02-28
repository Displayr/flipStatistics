#' \code{CosineSimilarities}
#'
#' @description Computes the cosine similarity between columns of a matrix. The cosine similarity
#' is the cosine of the angle between vectors (columns).
#'
#' @param x A \code{matrix} whose columns we use to calculate the cosine similarity.
#' @param weight A \code{vector} of weights.
#' @importFrom verbs Sum SumEachColumn
#' @export
CosineSimilarities <- function(x, weight = NULL)
{
    if (is.null(weight))
        weight <- rep(1, nrow(x))
    sum.x.squared <- SumEachColumn(x ^ 2 * weight, remove.rows = NULL, remove.missing = FALSE)
    n <- ncol(x)
    result <- matrix(NA, n, n)
    for (i in 1:n) {
        for (j in i:n) {
            if (i != j) {
                result[i, j] <- verbs::Sum(x[, i] * x[, j] * weight, remove.missing = FALSE) / sqrt(sum.x.squared[i] * sum.x.squared[j])
                result[j, i] <- result[i, j]
            } else {
                result[i, i] <- 1
            }
        }
    }
    colnames(result) <- colnames(x)
    rownames(result) <- colnames(x)
    result
}
