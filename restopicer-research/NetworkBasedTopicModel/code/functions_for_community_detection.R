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
clique.percolation.community <- function(graph, k, threshold = 1,cutat=NULL) {
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
# percolation on the nodes directly, not good and right
linkcomm.percolation.community <- function(g_edgelist,bipartite=FALSE,dist=NULL,threshold=0.15,cutat=NULL){
  if(file.exists("rdata/tmp/lc.RData")){
    load(file = "rdata/tmp/lc.RData")
  }else{
    lc <- getLinkCommunities(g_edgelist,hcmethod="average",bipartite=bipartite,dist = dist,plot = F)
    save(lc,file = "rdata/tmp/lc.RData")
  }
  if(!is.null(cutat)){
    lc <- newLinkCommsAt(lc, cutat = cutat)
  }
  if(bipartite){
    
  }else{
    community_member_list <- lapply(split(lc$nodeclusters$node,f = lc$nodeclusters$cluster),FUN = function(x){unlist(as.character(x))})
    if(file.exists("rdata/tmp/lc_percolation_sim.RData")){
      load(file = "rdata/tmp/lc_percolation_sim.RData")
    }else{
      comm_pair_df <- data.frame()
      for (i in 1:length(community_member_list)) {
        for (j in i:length(community_member_list)) {
          comm_i <- community_member_list[[i]]
          comm_j <- community_member_list[[j]]
          num_support <- length(intersect(comm_i,comm_j))
          num_anti_i <- length(setdiff(comm_i,comm_j))
          num_anti_j <- length(setdiff(comm_j,comm_i))
          # method A: num_support/max(num_anti_i,num_anti_j) >= threshold used
          # method B: num_support/(num_anti_i+num_anti_j) >= threshold
          sim=num_support/max(num_anti_i,num_anti_j)
          if(sim>0)comm_pair_df <- rbind(comm_pair_df,data.frame(i,j,sim))
        }
      }
      save(comm_pair_df,file = "rdata/tmp/lc_percolation_sim.RData")
    }
    if(is.null(cutat)){
      linkcomm.graph <- simplify(graph_from_edgelist(as.matrix(comm_pair_df[comm_pair_df$sim >= threshold,c("i","j")]),directed = F))
      V(linkcomm.graph)$name <- seq_len(vcount(linkcomm.graph))
      comm_comps <- decompose.graph(linkcomm.graph)
      result <- lapply(comm_comps, function(x) {
        unique(unlist(community_member_list[ V(x)$name ]))
      })
    }else{
      result <- community_member_list
      if(length(community_member_list)<=cutat)return(result)
      rm_list <- c()
      for(step in 1:(length(community_member_list)-cutat)){
        comm_pair_df <- comm_pair_df[comm_pair_df$sim!=Inf,]
        c_pair <- comm_pair_df[which.max(comm_pair_df$sim)[1],]
        result[[c_pair$i]] <- unique(unlist(result[c(c_pair$i,c_pair$j)]))
        rm_list <- c(rm_list,c_pair$j)
        # fix similarity
        comm_pair_df <- comm_pair_df[!(comm_pair_df$i %in% c(c_pair$i,c_pair$j))|(comm_pair_df$j %in% c(c_pair$i,c_pair$j)),]
        i <- c_pair$i
        for (j in 1:length(result)) {
          if(result[[j]] %in% rm_list)next
          comm_i <- result[[i]]
          comm_j <- result[[j]]
          num_support <- length(intersect(comm_i,comm_j))
          num_anti_i <- length(setdiff(comm_i,comm_j))
          num_anti_j <- length(setdiff(comm_j,comm_i))
          sim=num_support/max(num_anti_i,num_anti_j)
          if(sim>0)comm_pair_df <- rbind(comm_pair_df,data.frame(i,j,sim))
        }
      }
      result <- result[-rm_list]
    }
  }
  return(result)
}
# percolation using the nodes on the edges/graph, which is good and right
linkcomm.percolation <- function(g_edgelist,bipartite=FALSE,dist=NULL,threshold=0.15,cutat_th=NULL){
  if(file.exists("rdata/tmp/lc.RData")){
    load(file = "rdata/tmp/lc.RData")
  }else{
    lc <- getLinkCommunities(g_edgelist,hcmethod="average",bipartite=bipartite,dist = dist,plot = F)
    save(lc,file = "rdata/tmp/lc.RData")
  }
  if(!is.null(cutat_th)){
    if(file.exists(paste("rdata/tmp/","lc_cutat=",cutat_th,".RData",sep=""))){
      load(file = paste("rdata/tmp/","lc_cutat=",cutat_th,".RData",sep=""))
    }else{
      lc <- newLinkCommsAt(lc, cutat = cutat_th)
      save(lc,file = paste("rdata/tmp/","lc_cutat=",cutat_th,".RData",sep=""))
    }
  }
  if(bipartite){
    
  }else{
    community_node_list <- lapply(split(lc$nodeclusters$node,f = lc$nodeclusters$cluster),FUN = function(x){unlist(as.character(x))})
    if(file.exists(paste("rdata/tmp/","lc_cutat=",cutat_th,"_sim.RData",sep=""))){
      load(file = paste("rdata/tmp/","lc_cutat=",cutat_th,"_sim.RData",sep=""))
    }else{
      comm_pair_df <- data.frame()
      for (i in 1:length(community_node_list)) {
        for (j in i:length(community_node_list)) {
          comm_i <- community_node_list[[i]]
          comm_j <- community_node_list[[j]]
          num_support <- length(intersect(comm_i,comm_j))
          num_anti_i <- length(setdiff(comm_i,comm_j))
          num_anti_j <- length(setdiff(comm_j,comm_i))
          # method A: num_support/max(num_anti_i,num_anti_j) >= threshold used
          # method B: num_support/(num_anti_i+num_anti_j) >= threshold
          # method C: num_support/(num_anti_i+num_support+num_anti_j) >= threshold
          sim=num_support/max(num_anti_i,num_anti_j)
          if(sim>0)comm_pair_df <- rbind(comm_pair_df,data.frame(i,j,sim))
        }
      }
      save(comm_pair_df,file = paste("rdata/tmp/","lc_cutat=",cutat_th,"_sim.RData",sep=""))
    }
    linkcomm.graph <- simplify(graph_from_edgelist(as.matrix(comm_pair_df[comm_pair_df$sim >= threshold,c("i","j")]),directed = F))
    V(linkcomm.graph)$name <- seq_len(vcount(linkcomm.graph))
    comm_comps <- decompose.graph(linkcomm.graph)
    result <- lapply(comm_comps, function(x) {
      unique(unlist(lc$clusters[ V(x)$name ]))
    })
  }
  return(result)
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