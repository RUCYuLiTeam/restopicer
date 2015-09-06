year<-1994
pattern <- paste(year,'-*-*', sep = "")
group <- grep(pattern, Cit.HepPh.WithDates.WellCited[,2], 
              ignore.case = FALSE, perl = FALSE, value = FALSE, 
              fixed = FALSE, useBytes = FALSE, invert = FALSE)
graphDY<-Cit.HepPh.WithDates.WellCited[group,c(1,3)]

gg <- graph.data.frame(d = graphDY, directed = T)
is.simple(gg)
gg <- simplify(gg)

is.connected(gg, mode=c("weak"))
gc<-clusters(gg, mode=c("weak"))
V(gg)$sg = gc$membership
V(gg)$color = rainbow(max(V(gg)$sg))[V(gg)$sg]

comps <- decompose.graph(gg,max.comps=1,min.vertices=max(gc$csize))
g<-comps[[1]]

com5 = walktrap.community(g, steps = 10)
com5$vcount
length(com5)
max(sizes(com5))

A<-com$names[com$membership==1]
B1<-com2$names[com2$membership==4]
B2<-com2$names[com2$membership==5]
B3<-com2$names[com2$membership==11]
B4<-com2$names[com2$membership==12]
B5<-com2$names[com2$membership==18]

venn.diagram(list(y=A,x5=B5,x4=B4,x3=B3,x2=B2),
             paste('out.tiff',sep = ""))
venn.diagram(list(y=A,x5=B5,x3=B3),
             paste('out.tiff',sep = ""))

V(g)$sg = com2$membership
V(g)$color = rainbow(max(V(g)$sg))[V(g)$sg]
g
## png("net_walktrap.png", width = 500, height = 500)
par(mar = c(0, 0, 0, 0))
set.seed(23)
plot(delete.vertices(g,com2$membership!=18), layout = layout.fruchterman.reingold, 
     vertex.size = 3,vertex.label.cex=0.5, vertex.label=NA,vertex.color = V(g)$color,
     edge.color = grey(0.5), edge.arrow.mode = "->",edge.arrow.size=0.2)
## dev.off()