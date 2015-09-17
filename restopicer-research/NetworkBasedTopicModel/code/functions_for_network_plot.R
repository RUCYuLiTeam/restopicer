######
# plot network for bipartite and co-term
# type : matrix and edgelist
######
plotBipartiteNetworkReport <- function(filename,bi_graph,community_member_list=NULL,showNamesInPlot = TRUE,path = "output/"){
  # folder
  if(!file.exists(path)) dir.create(path,recursive = TRUE)
  if(showNamesInPlot){
    label <- NULL
  }else{
    label <- NA
  }
  png(file.path(path,paste(filename,"bipartite.png",sep="-")),width=2000,height=2000,pointsize=2)
  par(mar = c(0, 0, 0, 0))
  set.seed(13)
  if(is.null(community_member_list)){
    plot(bi_graph,layout=layout_as_bipartite(bi_graph,hgap = 2),vertex.label=label,
         vertex.color="steelblue",vertex.size = 2,vertex.label.color="brown",
         vertex.label.cex=2,vertex.label.font=2,vertex.label.dist=0,
         edge.color="darkgrey",edge.label.cex=1,edge.width=1.8)
  }else{
    comm_color <- rainbow(length(community_member_list), alpha = 0.3)
    plot(bi_graph,layout=layout_as_bipartite(bi_graph,hgap = 2),vertex.label=label,mark.shape=1,
         mark.groups=community_member_list,mark.col=comm_color,mark.expand=5,
         vertex.color="steelblue",vertex.size = 2,vertex.label.color="brown",
         vertex.label.cex=5,vertex.label.font=2,vertex.label.dist=0,
         edge.color="darkgrey",edge.label.cex=1,edge.width=1)
  }
  dev.off()
}
plotTopicNetworkReport <- function(filename,graph,community_member_list=NULL,showNamesInPlot = TRUE,plotCommunity=FALSE,plotOverallTopics=FALSE,path = "output/"){
  # folder
  if(!file.exists(path)) dir.create(path,recursive = TRUE)
  if(showNamesInPlot){
    label <- NULL
  }else{
    label <- NA
  }
  if(plotCommunity){
    # plot for each community
    for(i in 1:length(community_member_list)){
      g <- delete.vertices(graph,names(V(graph))[!names(V(graph)) %in% community_member_list[[i]]])
      png(file.path(path,paste(filename,i,"topicnetwork.png",sep="-")),width=600,height=600,pointsize=6)
      par(mar = c(5, 4, 4, 2))
      set.seed(13)
      plot(g,layout=layout.auto,vertex.label=label,
           vertex.color="steelblue",vertex.size = 2,vertex.label.color="brown",
           vertex.label.cex=2,vertex.label.font=2,vertex.label.dist=0,
           edge.color="darkgrey",edge.label.cex=1,edge.width=1.8)
      dev.off()
    }
  }
  if(plotOverallTopics){
    png(file.path(path,paste(filename,"network.png",sep="-")),width=1000,height=1000,pointsize=2)
    par(mar = c(3, 2, 0, 2))
    set.seed(13)
    comm_color <- rainbow(length(community_member_list), alpha = 0.3)
    plot(graph,layout=layout.auto,vertex.label=label,mark.shape=1,
         mark.groups=community_member_list,mark.col=comm_color,mark.expand=5,
         vertex.color="steelblue",vertex.size = 2,vertex.label.color="brown",
         vertex.label.cex=2,vertex.label.font=2,vertex.label.dist=0,
         edge.color="darkgrey",edge.label.cex=1,edge.width=1.8)
    dev.off()
  }
}
