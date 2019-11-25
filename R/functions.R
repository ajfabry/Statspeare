#' Calculate the dot product of two vectors.
#'
#' @param x A vector.
#' @param y A vector.
#' @return The dot product of \code{x} and \code{y}.
#' @examples
#' dot_product(c(1,2,3,4), c(5,6,7,8))
dot_product = function(x, y) {
  results = sum(x * y, na.rm = T)
  return(results)
}

#' Calculate the weighted mean value of a vector.
#'
#' @param x A vector.
#' @param weights An optional vector of weights.
#' @return The weighted mean of \code{x}.
#' @examples
#' avg_mean(c(1,2,3,4))
#' avg_mean(c(1,2,3,4), c(0.1, 0.2, 0.3, 0.4))
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
st_deviation = function(x, mode = "biased") {
  results = sqrt(variance(x, mode = mode))
  return(results)
}

#' Calculate the covariance of a vector.
#'
#' @param x A vector.
#' @param mode Biased (default) or unbiased.
#' @return Coariance of \code{x}.
#' @examples
#' covariance(c(1,2,3,4))
#' covariance(c(1,2,3,4,), "unbiased")
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
correlation = function(x, y) {
  results = covariance(x, y) / sqrt(variance(x) * variance(y))
  return(results)
}

#' Calculate the words most correlated with an input word.
#'
#' @param m A matrix.
#' @param word An entry in \code{m}.
#' @param num_results Number of results to display.
#' @return A data table of words in \code{m} most correlated with \code{x}.
#' @examples
#' most_correlated(W, "loue")
most_correlated = function(m, word, num_results = 10) {
  if (length(word) == 1) {
    vec = m[word,]
  } else {
    vec = word
  }
  results = apply(m, 1, correlation, vec)
  return(sort(results, decreasing = T)[1:num_results])
}

#' Calculate the z-score of all elements in a vector.
#'
#' @param x A vector.
#' @return The z-score of all elements in \code{x}.
#' @examples
#' zscore(c(1,2,3,4))
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
entropy = function(x, normalize = F) {
  p = x / sum(x)
  if (normalize == T) {
    results = -1 * sum(p * log(p, base = length(p)), na.rm = T)
  } else {
    results = -1 * sum(p * log(p), na.rm = T)
  }
  return(results)
}

#' Calculate the relative entropy of two vectors.
#'
#' @param x A vector.
#' @param y A vector
#' @return The relative entropy of \code{x} and \code{y}.
#' @examples
#' relative_entropy(c(1,2,3,4), c(5,6,7,8))
relative_entropy = function(x, y) {
  p = x / sum(x)
  q = y / sum(y)
  results = sum(p * log(p / q), na.rm = T)
  return(results)
}

#' Calculate positive pointwise mutual information of an array..
#'
#' @param A A matrix.
#' @return PPMI of all elements in \code{A}.
#' @examples
#' ppmi(A)
ppmi = function(A) {
  pxy = A / sum(A)
  pxpy = rowSums(pxy) %*% t(colSums(pxy))
  PPMI = log(pxy / pxpy)
  PPMI[PPMI < 0] = 0
  PPMI[is.na(PPMI)] = 0
  return(PPMI)
}

#' Calculate the least divergent elements from a word.
#'
#' @param m A matrix.
#' @param word An elements of the matrix.
#' @param num_results Number of results to display.
#' @return A data table of elements in \code{m} least divergent from \code{word}.
#' @examples
#' least_divergent(W, "loue")
#' least_divergent(W, "loue", num_results = 20)
least_divergent = function(m, word, num_results = 10) {
  results = apply(m, 1, relative_entropy, m[word,])
  return(sort(results, decreasing = F)[1:num_results])
}

#' Calculate the cosine similarity between two vectors.
#'
#' @param x A vector.
#' @param y A vector.
#' @return Cosine similarity between \code{x} and \code{y}.
#' @examples
#' cosine_similarity(c(1,2,3,4), c(5,6,7,8))
cosine_similarity = function(x, y) {
  results = x %*% y/(sqrt(x %*% x) * sqrt(y %*% y))
  return(results)
}

#' Calculate the words most similar to an input word.
#'
#' @param m A matrix.
#' @param word An entry in \code{m}.
#' @param num_results Number of results to display.
#' @return A data table of words in \code{m} most similar to \code{x}.
#' @examples
#' most_similar(W, "loue")
most_similar = function(m, word, num_results = 10) {
  if (length(word) == 1) {
    vec = m[word,]
  } else {
    vec = word
  }
  results = apply(m, 1, cosine_similarity, vec)
  return(sort(results, decreasing = T)[1:num_results])
}

