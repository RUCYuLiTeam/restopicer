library(igraph)
library(ggplot2)
g = graph.data.frame(d = Cit.HepPh.WithDates[,c(1,3)], directed = T)
#total
dg_DATA<-data.frame(percent=degree.distribution(graph=g,mode="total"))
myplot<-ggplot(dg_DATA,aes(x=c(0:(length(percent)-1)),y=percent))
myplot+geom_point()+labs(x='total degree', y='percentage of total nodes')

dg_DATA<-data.frame(percent=degree.distribution(graph=g,mode="total"))
myplot<-ggplot(dg_DATA,aes(x=log10(c(0:(length(percent)-1))),y=percent))
myplot+geom_point()+labs(x='total degree', y='percentage of total nodes')

dg_DATA<-data.frame(percent=degree.distribution(graph=g,mode="total"))
myplot<-ggplot(dg_DATA,aes(x=log10(c(0:(length(percent)-1))),y=log10(percent)))
myplot+geom_point()+labs(x='total degree (log10)', y='percentage of total nodes (log10)')

#in
dg_DATA<-data.frame(percent=degree.distribution(graph=g,mode="in"))
myplot<-ggplot(dg_DATA,aes(x=c(0:(length(percent)-1)),y=percent))
myplot+geom_point()+labs(x='in degree', y='percentage of total nodes')

dg_DATA<-data.frame(percent=degree.distribution(graph=g,mode="in"))
myplot<-ggplot(dg_DATA,aes(x=log10(c(0:(length(percent)-1))),y=percent))
myplot+geom_point()+labs(x='in degree', y='percentage of total nodes')

dg_DATA<-data.frame(percent=degree.distribution(graph=g,mode="in"))
myplot<-ggplot(dg_DATA,aes(x=log10(c(0:(length(percent)-1))),y=log10(percent)))
myplot+geom_point()+labs(x='in degree (log10)', y='percentage of total nodes (log10)')

#out
dg_DATA<-data.frame(percent=degree.distribution(graph=g,mode="out"))
myplot<-ggplot(dg_DATA,aes(x=c(0:(length(percent)-1)),y=percent))
myplot+geom_point()+labs(x='out degree', y='percentage of total nodes')

dg_DATA<-data.frame(percent=degree.distribution(graph=g,mode="out"))
myplot<-ggplot(dg_DATA,aes(x=log10(c(0:(length(percent)-1))),y=percent))
myplot+geom_point()+labs(x='out degree', y='percentage of total nodes')

dg_DATA<-data.frame(percent=degree.distribution(graph=g,mode="out"))
myplot<-ggplot(dg_DATA,aes(x=log10(c(0:(length(percent)-1))),y=log10(percent)))
myplot+geom_point()+labs(x='out degree (log10)', y='percentage of total nodes (log10)')

dg_in = degree(g,mode="in")
percent_in=degree.distribution(graph=g,mode="in")
degree<-function(per_in){
  length(percent_in[percent_in > per_in])
}
degree(0.005)