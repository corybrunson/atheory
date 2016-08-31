#' q-connectivity graph for a simplicial complex
#' 
#' Given a simplicial complex and a dimension q, construct the q-connectivity 
#' graph, which is the graph whose nodes are the maximal simplices of the 
#' complex and whose edges link those pairs of simplices that share a face of 
#' dimension at least q.
#' 
#' @param sc The input simplicial complex, represented as a list of simplicies, 
#'   each a vector of positive integers representing the nodes.
#' @param q A positive integer, the minimal dimension of a face two simplices
#'   must share in order to be connected.
#' @import igraph
#' @export
#' @example inst/examples/sc-to-graph.r
sc_to_graph <- function(sc, q) {
  edges <- c()                               # empty edge vector
  sc.q <- sc[which(sapply(sc, length) > q)]  # list of q-maximal simplices
  sc.q <- unique(sc.q)                       # in case of duplicates
  sc.q <- lapply(sc.q, sort)                 # for consistency in names
  sc.names <- sapply(sc.q, function(lst) {   # vector of simplex names,
    paste(lst, collapse = ' ')               #   derived from vertex numbers
  })
  n <- length(sc.q)                          # number of q-maximal simplices
  if(n == 0) return(make_empty_graph(n = 0)) # if no q-dimensional simplices
  if(length(sc.q) > 1) for(i in 1:(n - 1)) for(j in (i + 1):n) {
    if(length(intersect(sc.q[[i]], sc.q[[j]])) > q)  # across 1 <= i < j <= n,
      edges <- c(edges, i, j)                          #   if simplices share q+1
  }                                                  #   vertices, link them
  sc.g <- graph(edges, n = n, directed = F)  # keep solo simplices in graph
  V(sc.g)$name <- sc.names                   # assign names to vertices
  sc.g
}
