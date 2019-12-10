#' Calculate the dot product of two vectors.
#'
#' @param x A vector.
#' @param y A vector.
#' @return The dot product of \code{x} and \code{y}.
#' @examples
#' dot_product(c(1,2,3,4), c(5,6,7,8))
#' dot_product(W["loue",], W["hate",])
#' @export
dot_product = function(x, y) {
  results = sum(x * y, na.rm = T)
  return(results)
}

#' Calculate the covariance of two vectors.
#'
#' @param x A vector.
#' @param mode Biased (default) or unbiased.
#' @return Coariance of \code{x}.
#' @examples
#' covariance(c(1,2,3,4))
#' covariance(c(1,2,3,4,), "unbiased")
#' @export
covariance = function(x, y, mode = "biased") {
  n = length(x)
  residuals_x = x - avg_mean(x)
  residuals_y = y - avg_mean(y)
  if (mode == "biased") {
    results = dot_product(residuals_x, residuals_y) / n
  }
  if (mode == "unbiased") {
    results = dot_product(residuals_x, residuals_y) / (n - 1)
  }
  return(results)
}

#' Calculate the correlation between two vectors.
#'
#' @param x A vector.
#' @param y A vector.
#' @return Correlation between \code{x} and \code{y}.
#' @examples
#' correlation(c(1,2,3,4), c(1,2,3,5))
#' @export
correlation = function(x, y) {
  results = covariance(x, y) / sqrt(variance(x) * variance(y))
  return(results)
}

#' Calculate the relative entropy of two vectors.
#'
#' @param x A vector.
#' @param y A vector
#' @return The relative entropy of \code{x} and \code{y}.
#' @examples
#' relative_entropy(c(1,2,3,4), c(5,6,7,8))
#' @export
relative_entropy = function(x, y) {
  p = x / sum(x)
  q = y / sum(y)
  results = sum(p * log(p / q), na.rm = T)
  return(results)
}

#' Calculate the cosine similarity between two vectors.
#'
#' @param x A vector.
#' @param y A vector.
#' @return Cosine similarity between \code{x} and \code{y}.
#' @examples
#' cosine_similarity(c(1,2,3,4), c(5,6,7,8))
#' @export
cosine_similarity = function(x, y) {
  results = x %*% y/(sqrt(x %*% x) * sqrt(y %*% y))
  return(results)
}
