# cycle graph
g <- graph(c(1,2, 2,3, 3,4, 4,5, 5,1), directed = FALSE)
a1rk(g)

# two-cycle graph
h <- add_vertices(g, 4)
h <- add_edges(h, c(2,6, 6,7, 7,8, 8,9, 9,1))
a1rk(h)

# Platonic solids
a1rk(graph.famous("tetrahedron"))
a1rk(graph.famous("cubical"))
a1rk(graph.famous("dodecahedron"))
