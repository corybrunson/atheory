#' Remove non-maximal simplices from a simplicial complex
#' 
#' @param sc The input simplicial complex, represented as a list of simplicies, 
#'   each a vector of positive integers representing the nodes.
#' @import igraph
#' @export
top_sc <- function(sc) {
  len <- sapply(sc, length)                # vertex counts of simplices
  val <- setdiff(
    sort(unique(len), decreasing = TRUE),  # decreasing vector of distinct
    max(len)                               #   simplex counts, except top
  )
  subs <- c()                              # indices of simplices to drop
  for(m in val) {
    w <- which(len == m)                   # simplices of count m
    ww <- setdiff(which(len > m), subs)    # simplices of count > m
    subs <- c(subs, w[which(sapply(        # append to `subs' the indices of
      sc[w],                               #   simplices that have count m
      function(wlst) any(sapply(           #   and for some larger simplex
        sc[ww], function(wwlst)            #   have vertex set
          all(is.element(wlst, wwlst))     #   contained in its vertex set
      ))
    ))])
  }
  sc[setdiff(1:length(sc), subs)]          # return list of kept simplices
}
