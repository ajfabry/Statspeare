#' Calculate the weighted mean value of a vector.
#'
#' @param x A vector.
#' @param weights An optional vector of weights.
#' @return The weighted mean of \code{x}.
#' @examples
#' avg_mean(c(1,2,3,4))
#' avg_mean(c(1,2,3,4), c(0.1, 0.2, 0.3, 0.4))
#' @export
avg_mean = function(x, weights = c()) {
  n = length(x)
  if (length(weights) == 0) {
    weights = rep(1/n, n)
  }
  results = dot_product(x, weights)
  return(results)
}

#' Calculate the variance of a vector.
#'
#' @param x A vector.
#' @param mode Biased (default) or unbiased.
#' @return Variance of \code{x}.
#' @examples
#' variance(c(1,2,3,4))
#' variance(c(1,2,3,4,), "unbiased")
#' @export
variance = function(x, mode = "biased") {
  n = length(x)
  residuals_x = x - avg_mean(x)
  if (mode == "biased") {
    results = dot_product(residuals_x, residuals_x) / n
  }
  if (mode == "unbiased") {
    results = dot_product(residuals_x, residuals_x) / (n-1)
  }
  return(results)
}

#' Calculate the standard deviation of all elements in a vector.
#'
#' @param x A vector.
#' @param mode Biased (default) or unbiased.
#' @return A vector of the standard deviation of each element in \code{x}.
#' @examples
#' st_deviation(c(1,2,3,4))
#' st_deviation(c(1,2,3,4,), "unbiased")
#' @export
st_deviation = function(x, mode = "biased") {
  results = sqrt(variance(x, mode = mode))
  return(results)
}

#' Calculate the z-score of all elements in a vector.
#'
#' @param x A vector.
#' @return The z-score of all elements in \code{x}.
#' @examples
#' zscore(c(1,2,3,4))
#' @export
zscore = function(x) {
  results = (x - avg_mean(x)) / st_deviation(x)
  return(results)
}

#' Calculate the entropy of a vector.
#'
#' @param x A vector.
#' @param normalize True or false (default).
#' @return The entropy of \code{x}.
#' @examples
#' entropy(c(1,2,3,4))
#' entropy(c(1,2,3,4), T)
#' @export
entropy = function(x, normalize = F) {
  p = x / sum(x)
  if (normalize == T) {
    results = -1 * sum(p * log(p, base = length(p)), na.rm = T)
  } else {
    results = -1 * sum(p * log(p), na.rm = T)
  }
  return(results)
}

#' Calculate positive pointwise mutual information of an array.
#'
#' @param A A matrix.
#' @return PPMI of all elements in \code{A}.
#' @examples
#' ppmi(A)
#' @export
ppmi = function(A) {
  pxy = A / sum(A)
  pxpy = rowSums(pxy) %*% t(colSums(pxy))
  PPMI = log(pxy / pxpy)
  PPMI[PPMI < 0] = 0
  PPMI[is.na(PPMI)] = 0
  return(PPMI)
}

