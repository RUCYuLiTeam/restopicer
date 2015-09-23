######
# self-defined community detection method or similarity calculations
# 1.self-defined community detection method
# 2.self-defined similarity method
######
# Community Detection In R
# igraph
# http://igraph.wikidot.com/community-detection-in-r
# g <- sample_gnp(100, 0.3)
# k <- 6
clique.percolation.community <- function(graph, k, threshold = 1) {
  clq <- cliques(graph, min=k, max=k)
  edges <- c()
  for (i in 1:length(clq)) {
    for (j in i:length(clq)) {
      # (K-threshold)/(K+threshold)
      if ( length(unique(c(clq[[i]], clq[[j]]))) <= k+threshold ) {
        edges <- c(edges, c(i,j))
      }
    }
  }
  clq.graph <- simplify(graph(edges))
  V(clq.graph)$name <- seq_len(vcount(clq.graph))
  comps <- decompose.graph(clq.graph)
  
  lapply(comps, function(x) {
    unique(unlist(clq[ V(x)$name ]))
  })
}
linkcomm.percolation.community <- function(g_edgelist,bipartite=FALSE,dist=NULL,threshold=0.5){
  lc <- getLinkCommunities(g_edgelist,hcmethod="average",bipartite=bipartite,dist = dist,plot = F)
  community_member_list <- lapply(split(lc$nodeclusters$node,f = lc$nodeclusters$cluster),FUN = function(x){unlist(as.character(x))})
  comm_pair <- c()
  for (i in 1:length(community_member_list)) {
    for (j in i:length(community_member_list)) {
      comm_i <- community_member_list[[i]]
      comm_j <- community_member_list[[j]]
      num_support <- length(intersect(comm_i,comm_j))
      num_anti_i <- length(setdiff(comm_i,comm_j))
      num_anti_j <- length(setdiff(comm_j,comm_i))
      # method A: num_support/max(num_anti_i,num_anti_j) >= threshold
      # method B: num_support/(num_anti_i+num_anti_j) >= threshold
      if ( num_support/max(num_anti_i,num_anti_j) >= threshold ) {
        comm_pair <- c(comm_pair, c(i,j))
      }
    }
  }
  linkcomm.graph <- simplify(graph(comm_pair,directed = F))
  V(linkcomm.graph)$name <- seq_len(vcount(linkcomm.graph))
  comm_comps <- decompose.graph(linkcomm.graph)
  lapply(comm_comps, function(x) {
    unique(unlist(community_member_list[ V(x)$name ]))
  })
}
largeScaleCommunity <- function(g,mode="all"){
  V(g)$group <- as.character(V(g))
  thisOrder <- sample(vcount(g),vcount(g))-1
  t <- 0
  done <- FALSE
  while(!done){
    t <- t+1
    cat("\rtick:",t)
    done <- TRUE ## change to FALSE whenever a node changes groups              
    for(i in thisOrder){
      ## get the neighbor group frequencies:                                    
      groupFreq <- table(V(g)[neighbors(g,i,mode=mode)]$group)
      ## pick one of the most frequent:                                         
      newGroup <- sample(names(groupFreq) [groupFreq==max(groupFreq)],1)
      if(done){done <- newGroup==V(g)[i]$group}
      V(g)[i]$group <- newGroup
    }
  }
  ## now fix any distinct groups with same labels:                              
  for(i in unique(V(g)$group)){
    ## only bother for connected groups                                         
    if(!is.connected(subgraph(g,V(g)[group==i]))){
      theseNodes <- V(g)[group==i]
      theseClusters <- clusters(subgraph(g,theseNodes))
      ## iterate through the clusters and append their names                    
      for(j in unique(theseClusters$membership)){
        V(g)[theseNodes[theseClusters$membership==j]]$group <- paste(i,j,sep=".")
      }
    }
  }
  return(g)
}