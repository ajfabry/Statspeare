#' Returns the words most associated with a keyword.
#'
#' @description \code{top_relations} takes in a matrix, word, association method, and number of
#' results and outputs a sorted list of words most associated with the input word based on the
#' specified association method.
#' @description For a description of each method, type \code{?<method>} in the console.
#' @param m A matrix.
#' @param word A word.
#' @param method "most correlated," "most similar," or "least divergent."
#' @param num_results An integer.
#' @return Top words associated with \code{word} in matrix \code{m}.
#' @examples
#' top_relations(W, "loue", "most correlated", 20)
#' @export
top_relations = function(m, word, method = "most correlated", num_results = 10) {
  if(method == "least divergent") {
    results = least_divergent(m, word, num_results)
  } else if(method == "most similar") {
    results = most_similar(m, word, num_results)
  } else {
    results = most_correlated(m, word, num_results)
  }
  return(results)
}

#' Calculate the words most correlated with an input word.
#'
#' @description The correlation coefficient between vectors \code{x} and \code{y} is calculated
#' using \deqn{cor(x,y) = cov(x,y) / sqrt( var(x) * var(y) )} In this function, this formula is
#' applied between \code{x} all other word vectors in matrix \code{m}, and the resulting
#' correlation coefficients are sorted and returned in the form of a data table.
#' @param m A matrix.
#' @param word An entry in \code{m}.
#' @param num_results Number of results to display.
#' @return A data table of words in \code{m} most correlated with \code{x}.
#' @examples
#' most_correlated(W, "loue")
#' @references
#' “Displaying and Summarizing Data.” \emph{Introduction to Bayesian Statistics}, by William M.
#' Bolstad, 2nd ed., Wiley, 2007, pp. 29–54.
most_correlated = function(m, word, num_results = 10) {
  if (length(word) == 1) {
    vec = m[word,]
  } else {
    vec = word
  }
  results = apply(m, 1, correlation, vec)
  return(sort(results, decreasing = T)[1:num_results])
}

#' Calculate the least divergent elements from a word.
#'
#' @description Kullback-Leibler divergence, or relative entropy, between two vectors is
#' calculated as follows:
#' \deqn{D(p,q) = sum( p(x) * log( p(x) / q(x) ) )}
#' While similar to a measurement for distance between vectors, KL-divergence is not a true
#' measurement for distance; \eqn{D(p,q) != D(q,p)}.
#' @param m A matrix.
#' @param word An elements of the matrix.
#' @param num_results Number of results to display.
#' @return A data table of elements in \code{m} least divergent from \code{word}.
#' @examples
#' least_divergent(W, "loue")
#' least_divergent(W, "loue", num_results = 20)
#' @references
#' Cover, Thomas M., and Joy A. Thomas. \emph{Elements of Information Theory}.
#' Wiley-Interscience, 1991.
least_divergent = function(m, word, num_results = 10) {
  results = apply(m, 1, relative_entropy, m[word,])
  return(sort(results, decreasing = F)[1:num_results])
}

#' Calculate the words most similar to an input word.
#'
#' @description The similarity between two vectors is determined as follows:
#' \deqn{cos(x,y) = (a • b) / (||a|| ||b||)}
#' where (\code{a • b}) is equal to the dot product of \code{a} and \code{b}, and
#' (\code{||a|| ||b||}) is equal to the magnitude of \code{a} and \code{b}. This formula returns
#' the angle between the two vectors.
#' @param m A matrix.
#' @param word An entry in \code{m}.
#' @param num_results Number of results to display.
#' @return A data table of words in \code{m} most similar to \code{x}.
#' @examples
#' most_similar(W, "loue")
#' @references
#' “Word-Vectors and Search Engines.” \emph{Geometry and Meaning}, by Dominic Widdows, CSLI
#' Publications, 2004, pp. 132–166.
most_similar = function(m, word, num_results = 10) {
  if (length(word) == 1) {
    vec = m[word,]
  } else {
    vec = word
  }
  results = apply(m, 1, cosine_similarity, vec)
  return(sort(results, decreasing = T)[1:num_results])
}
