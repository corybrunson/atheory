#' Rank of the A-1 homotopy group of a graph
#' 
#' Calculate the rank (the number of homotopy-inequivalent cycles) of the first 
#' discrete homotopy group of a graph.
#' 
#' @param graph The input graph. It will be treated as simple and undirected. A 
#'   submodule handles the case that \code{graph} is connected, and a wrapper 
#'   feeds this submodule the connected components of \code{graph}.
#' @param componentwise Logical, defaults to false. If true, returns a vector of
#'   the A-1 ranks of the connected components of \code{graph}. If false,
#'   returns the A-1 rank of \code{graph}, which is the sum of these ranks.
#' @import igraph
#' @export
#' @example inst/examples/a1rk.r
a1rk <- function(graph, componentwise = FALSE) {
  if (!is_simple(graph)) {
    warning("Graph is not simple; simplifying first.")
    graph <- simplify(graph)
  }
  if (is_directed(graph)) {
    warning("Graph is directed; undirecting first.")
    graph <- as.undirected(graph)
  }
  if (is_connected(graph)) {
    a1rk.connected(graph)
  } else {
    cl <- clusters(graph)
    a1rks <- sapply(1:cl$no, function(i) {
      indgraph <- induced_subgraph(graph, vids = which(cl$membership == i))
      a1rk.connected(indgraph)
    })
    if (componentwise) a1rks else sum(a1rks)
  }
}

a1rk.connected <- function(graph) {
  stopifnot(is_connected(graph))
  # Recursively remove all nodes of degree < 2
  v <- Inf
  g <- graph
  while(vcount(g) != v) {
    v <- vcount(g)
    g <- delete_vertices(g, which(degree(g) < 2))
  }
  if(ecount(g) <= 2) return(0)
  # Identify 3-cycles and indices of their edges
  c3 <- unique(cliques(g, 3, 3))
  e3 <- unique(unlist(lapply(c3, function(x) E(g)[x %--% x])))
  # Identify 4-cycles and indices of their edges
  c4 <- unique(subgraph_isomorphisms(
    graph(c(1,2, 2,3, 3,4, 4,1), directed = FALSE),
    g,
    method = "vf2"
  ))
  e4 <- lapply(c4, function(x) E(g)[x %--% x])
  c4 <- c4[sapply(e4, length) == 4]
  e4 <- unique(unlist(e4[sapply(e4, length) == 4]))
  # Construct a subgraph h by removing all of these edges
  h <- delete_edges(g, setdiff(E(g), unique(c(e3, e4))))
  # Check that g and h contain the same numbers of 3- and 4-cycles
  stopifnot(simple_triad_census(g)[4] == simple_triad_census(h)[4])
  stopifnot(motifs(g, size = 4)[9] == motifs(h, size = 4)[9])
  # Decompose h into connected components
  cl <- clusters(h)
  # Initialize rank of A_1 of h
  a1rkh <- 0
  # Determine the rank of the A_1 group of each component of h
  for(i in which(cl$csize > 2)) {
    # Specialize cycle and edge lists to component i
    # (Need to use given list of edges of h for consistency & dimension of mat)
    ch <- c(c3[sapply(c3, function(vec) cl$membership[vec[1]]) == i],
            c4[sapply(c4, function(vec) cl$membership[vec[1]]) == i])
    # Initialize rank matrix
    mat <- matrix(0, nrow = ecount(h), ncol = length(ch))
    # Add a column for each cycle
    if(length(ch) > 0) for(j in 1:length(ch)) for(k in 1:length(ch[[j]])) {
      # Determine the index of the edge starting at ch[[j]][k]
      he <- E(h)[as.numeric(ch[[j]][k]) %--%
                   as.numeric(ch[[j]][k %% length(ch[[j]]) + 1])]
      # Determine whether the edge appears forward or backward in the cycle
      eva <- (ch[[j]][k] < ch[[j]][k %% length(ch[[j]]) + 1]) * 2 - 1
      # Assign these values to the corresponding matrix entries
      mat[he, j] <- eva
    }
    stopifnot(all(mat %in% -1:1))
    a1rkh <- a1rkh + qr(mat)$rank
  }
  # Subtract the sum of these ranks from the rank of the fundamental group of g
  1 - vcount(g) + ecount(g) - a1rkh
}
