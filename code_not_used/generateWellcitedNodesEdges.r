library(igraph)
library(ggplot2)
#library(sna)
#library(network)

g = graph.data.frame(d = Cit.HepPh.WithDates[,c(1,3)], directed = T)

dg_in = degree(g,mode="in")
percent_in = degree.distribution(graph=g,mode="in")

nodes<-function(degree){
  attributes(dg_in[dg_in > degree])$names
}
wellcited<-nodes(6)
length(wellcited)

j<-1
for(i in 1:nrow(Cit.HepPh.WithDates)){
  if(any(Cit.HepPh.WithDates[i,3]==wellcited)){
    Cit.HepPh.WithDates.WellCited[j,]<-Cit.HepPh.WithDates[i,]
    j=j+1
  }
}
