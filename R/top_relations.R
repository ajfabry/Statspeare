#' Returns the words most associated with a keyword.
#'
#' @param m A matrix.
#' @param word A word.
#' @param num_results An integer.
#' @param method "most_correlated," "most_similar," or "least_divergent."
#' @return Top words associated with \code{word} in matrix \code{m}.
#' @examples
#' top_relations(W, "loue", 20, "most_correlated")
#' @export
top_relations = function(m, word, num_results = 10, method = "most_correlated") {
  if(method == "least_divergent") {
    results = least_divergent(m, word, num_results)
  } else if(method == "most_similar") {
    results = most_similar(m, word, num_results)
  } else {
    results = most_correlated(m, word, num_results)
  }
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
