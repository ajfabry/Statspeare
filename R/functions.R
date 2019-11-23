dot_product = function(x, y) {
  results = sum(x * y, na.rm = T)
  return(results)
}

avg_mean = function(x, weights = c()) {
  n = length(x)
  if (length(weights) == 0) {
    weights = rep(1/n, n)
  }
  results = dot_product(x, weights)
  return(results)
}

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

st_deviation = function(x, mode = "biased") {
  results = sqrt(variance(x, mode = mode))
  return(results)
}

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

correlation = function(x, y) {
  results = covariance(x, y) / sqrt(variance(x) * variance(y))
  return(results)
}

most_correlated = function(m, word, num_results = 10) {
  if (length(word) == 1) {
    vec = m[word,]
  } else {
    vec = word
  }
  results = apply(m, 1, correlation, vec)
  return(sort(results, decreasing = T)[1:num_results])
}

zscore = function(x) {
  results = (x - avg_mean(x)) / st_deviation(x)
  return(results)
}

entropy = function(x, normalize = F) {
  p = x / sum(x)
  if (normalize == T) {
    results = -1 * sum(p * log(p, base = length(p)), na.rm = T)
  } else {
    results = -1 * sum(p * log(p), na.rm = T)
  }
  return(results)
}

relative_entropy = function(x, y) {
  p = x / sum(x)
  q = y / sum(y)
  results = sum(p * log(p / q), na.rm = T)
  return(results)
}

ppmi = function(A) {
  pxy = A / sum(A)
  pxpy = rowSums(pxy) %*% t(colSums(pxy))
  PPMI = log(pxy / pxpy)
  PPMI[PPMI < 0] = 0
  PPMI[is.na(PPMI)] = 0
  return(PPMI)
}

least_divergent = function(m, word, num_results = 10) {
  results = apply(m, 1, relative_entropy, m[word,])
  return(sort(results, decreasing = F)[1:num_results])
}

cosine_similarity = function(x, y) {
  results = x %*% y/(sqrt(x %*% x) * sqrt(y %*% y))
  return(results)
}

most_similar = function(m, word, num_results = 10) {
  if (length(word) == 1) {
    vec = m[word,]
  } else {
    vec = word
  }
  results = apply(m, 1, cosine_similarity, vec)
  return(sort(results, decreasing = T)[1:num_results])
}

