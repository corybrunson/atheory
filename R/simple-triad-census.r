#' Simple triad census
#' 
#' Calculate the triad census for an undirected graph by taking the appropriate 
#' sums from the triad census for a directed graph with the same edges (up to 
#' direction).
#' 
#' @param graph The input graph. It can be directed or undirected but the
#'   function will return the census for its undirected version.
#' @import igraph
simple_triad_census <- function(graph) {
  if (!is_simple(graph)) {
    warning("Graph is not simple; simplifying first.")
    graph <- simplify(graph)
  }
  digraph <- as.directed(graph, mode = "arbitrary")
  tc <- triad_census(digraph)
  c(
    tc[1], # no edges (empty)
    sum(tc[2:3]), # one edge
    sum(tc[c(4:8, 11)]), # two edges (vee/lambda)
    sum(tc[c(9:10, 12:16)]) # three edges (full/complete)
  )
}
