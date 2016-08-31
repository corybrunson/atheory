# This example is taken from Barcelo & Laubenbacher (2005)
# http://www.sciencedirect.com/science/article/pii/S0012365X05002323
sc1 <- list(
  c(1,2,3),
  c(1,3,4),
  c(1,4,5),
  c(1,5,6),
  c(1,6,2)
)
sc1_graph <- sc_to_graph(sc1, q = 1)
a1rk(sc1_graph)

# filling the combinatorial hole
sc2 <- c(sc1, list(c(1,2,5)))
sc2_graph <- sc_to_graph(sc2, q = 1)
a1rk(sc2_graph)
