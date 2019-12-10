#' Display the network graph of characters in a play.
#'
#' @description \code{show_graph} takes a play (\code{play}) and displays the network graph of
#' all character relationships in the play.
#' @param play The ID of a play.
#' @examples
#' show_graph("Rom")
#' @export
show_graph = function(play) {

  g = create_graph(play)

  plot(g, edge.arrow.size=0.5)

}
