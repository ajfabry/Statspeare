#' Create a network graph of characters in a play.
#'
#' @description \code{show_graph} takes a play (\code{play}) and creates the network graph of all character relationships in the play.
#' @param play The ID of a play.
#' @return The network graph of all character relationships in the play.
#' @examples
#' create_graph("Rom")
#' @export
create_graph = function(play) {

  relationships = colnames(character_metadata)[c(9:13, 16)]

  hits1 = grep(paste("_", play, sep=""), character_metadata$Character.ID)

  SOURCE = c()
  TARGET = c()
  RELATIONSHIP = c()
  edge_list = c()

  for (i in 1:length(relationships)) {

    relationship = relationships[i]

    hits2 = which(character_metadata[,relationship] != "")

    hits = intersect(hits1, hits2)

    selection = character_metadata[hits, c("Character.ID",relationship)]
    SOURCE = c(SOURCE, selection[,1])
    TARGET = c(TARGET, selection[,2])
    RELATIONSHIP = c(RELATIONSHIP, rep(relationship, length(hits)))

  }

  edge_list = cbind(SOURCE, TARGET)

  g = igraph::graph_from_edgelist(el = edge_list)

  ids = igraph::V(g)$name
  indices = c()
  for(id in ids) {
    indices = c(indices, match(id, character_metadata$Character.ID))
  }
  igraph::V(g)$name = character_metadata[indices, "Name"]
  igraph::V(g)$status = character_metadata[indices, "Status"]
  igraph::V(g)$gender = character_metadata[indices, "Gender"]
  igraph::V(g)$age = character_metadata[indices, "Age"]

  igraph::V(g)$color = as.factor(igraph::V(g)$gender)
  hits = which(igraph::V(g)$name != "")
  g = igraph::induced_subgraph(g, hits)

  return(g)

}
