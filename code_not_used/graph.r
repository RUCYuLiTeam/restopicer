library(igraph)
#gg = graph.data.frame(d = Cit.HepPh, directed = T)
gg = graph.data.frame(d = smallsize, directed = T)
is.simple(gg)
gg = simplify(gg)
is.simple(gg)
dg = degree(gg)

dg_in = degree(gg,mode="in")
dg_out = degree(gg,mode="out")
plot(degree.distribution(gg,mode="in"), xlab="node degree")

par(mar = c(0, 0, 0, 0))
set.seed(23)

plot(gg, layout = layout.fruchterman.reingold, 
     vertex.size = 3,vertex.label.cex=0.5, vertex.label=NA,
     edge.color = grey(0.5), edge.arrow.mode = "->",edge.arrow.size=0.2)