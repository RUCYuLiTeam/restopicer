Cit.HepPh.Unique<-unique(Cit.HepPh)
cit.HepPh.dates.Unique<-unique(cit.HepPh.dates)
cit.HepPh.dates.UniqueNode<-unique(cit.HepPh.dates$V1)

nrow(Cit.HepPh)
nrow(Cit.HepPh.Unique)

nrow(cit.HepPh.dates)
nrow(cit.HepPh.dates.Unique)
length(cit.HepPh.dates.UniqueNode)

dateOfPaper <- function(paperNodeId){
  cit.HepPh.dates.Unique[(cit.HepPh.dates.Unique$V1==paperNodeId),]$V2
}
dateOfPaper(112008)

library(igraph)
gg = graph.data.frame(d = Cit.HepPh.Unique, directed = T)
is.simple(gg)