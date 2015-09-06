#library(igraph)
library(sna)
#library(network)

#loading.attributes
x<-Cit.HepPh.WithDates[c(1:10000),c(1,3)]
x$val<-1
names(x)<-c('snd','rec','val')
g<-as.matrix(x)
#g <- graph.data.frame(d = x, directed = T)
#g<-network(x, matrix.type = 'edgelist', directed=TRUE)

#g<-rgraph(10,tprob=0.06)
#g<-as.edgelist.sna(g)
#Find weak components
cd<-component.dist(g,connected="weak")
cd$membership              #Who's in what component?
cd$csize                   #What are the component sizes?
#Plot the size distribution
plot(1:length(cd$cdist),cd$cdist/sum(cd$cdist),ylim=c(0,1),type="h")
lgc<-component.largest(g,connected="weak")  #Get largest component
gplot(g,vertex.col=2+lgc)  #Plot g, with component membership
#Plot largest component itself 

png(filename = "Rnetwork.png",
    width = 1160, height = 1160, units = "px", pointsize = 12,
    bg = "transparent")
gplot(component.largest(g,connected="weak",result="graph"))
dev.off()
