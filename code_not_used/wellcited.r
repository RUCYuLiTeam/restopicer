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

wellcitedpaperYear<-data.frame(year=1992:2002,paperCount=0)
dateOfPaper <- function(paperNodeId){
  cit.HepPh.dates.Unique[(cit.HepPh.dates.Unique$V1==paperNodeId),]$V2[1]
}
for(i in 1:length(wellcited)){
  year<-unlist(strsplit(as.character(dateOfPaper(wellcited[i])),split='-',fixed = TRUE))[1]
  wellcitedpaperYear[wellcitedpaperYear$year==year,]$paperCount <-
    wellcitedpaperYear[wellcitedpaperYear$year==year,]$paperCount+1
}

myplot<-ggplot(wellcitedpaperYear,aes(x=as.character(year),y=paperCount,fill=year))
myplot+geom_bar(stat="identity")+geom_point()+
  geom_text(aes(label=paperCount),hjust=0.5, vjust=-0.5 )+
  labs(x='year', y='WellcitedpaperCount')