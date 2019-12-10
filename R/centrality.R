#' Calculate the centralities of characters in a play.
#'
#' @description \code{centrality} takes a play (\code{play}) and a centrality measure
#' (\code{measure}) and returns the list of characters in the play sorted by the centrality
#' measure.
#' @param play The ID of a play.
#' @param measure "betweenness," "degree," or "eigenvector."
#' @return A data table of the sorted centrality value for all characters in the \code{play}.
#' @examples
#' centrality("Rom", "degree")
#' @export
centrality = function(play, measure) {

  g = create_graph(play)

  btw = sort(igraph::betweenness(g, directed = F), decreasing = T)
  deg = sort(igraph::degree(g, mode = "total"), decreasing = T)
  ec = sort(igraph::eigen_centrality(g, directed = F)$vector, decreasing = T)
  ec = round(ec, 7)

  if(measure == "betweenness") {
    return(btw)
  } else if(measure == "degree") {
    return(deg)
  } else if(measure == "eigenvector") {
    return(ec)
  } else {
    print("Error: unrecognized centrality measure")
  }

}
