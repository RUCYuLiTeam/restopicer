library(linkcomm)
library(igraph)
library(dplyr)
g <- matrix(1,nrow = 5,ncol = 5,byrow = T)
g_0 <- g
g_0[c(1,2,4),c(1,2,4)] <- 2
g_0[c(3,5),c(3,5)] <- 0
g_1 <- g_0*(g - diag(x = 1,nrow = 5,ncol = 5))

# g_2 <- simplify(graph.adjacency(g_1,mode = "undirected",weighted = TRUE))
# plot(g_2)
# 
# g_3_eid <- get.adjacency(g_2,type = "lower",edges = T,sparse = T)
# g_3 <- get.adjacency(g_2,type = "lower",edges = F,sparse = T)
# 
# g_4 <- get.edgelist(g_2,names = T)
g_s <- sparseMatrix(i = c(2,3,3,4,4,4,5,5,5,6,6,6,6,7,7,7),j = c(1,1,2,1,2,3,1,2,4,1,2,4,5,1,2,5),x = c(2,1,1,2,2,1,2,2,1,1,1,1,1,1,1,1),dims = c(7,7))
g_edges <- summary(g_s)
sim_df <- data.frame()
for(i in 1:(nrow(g_edges)-1)){
  e_i <- g_edges[i,]
  for(j in (i+1):nrow(g_edges)){
    e_j <- g_edges[j,]
    v_co <- intersect(c(e_i$i,e_i$j),c(e_j$i,e_j$j))
    #no_co <- g_edges$i!=v_co&g_edges$j!=v_co
    if(length(v_co)==1){
      v_i <- setdiff(c(e_i$i,e_i$j),c(e_j$i,e_j$j))
      v_j <- setdiff(c(e_j$i,e_j$j),c(e_i$i,e_i$j))
      # for a_i
      a_i <- array(data = 0,dim = dim(g_s)[1])
      tmp <- g_edges[(g_edges$i!=v_j&g_edges$j!=v_j)&(g_edges$i==v_i|g_edges$j==v_i),]
      tmp[tmp[,"j"]!=v_i,"i"] <- tmp[tmp[,"j"]!=v_i,"j"]
      a_i[tmp$i] <- tmp$x
      # for a_j
      a_j <- array(data = 0,dim = dim(g_s)[1])
      tmp <- g_edges[(g_edges$i!=v_i&g_edges$j!=v_i)&(g_edges$i==v_j|g_edges$j==v_j),]
      tmp[tmp[,"j"]!=v_j,"i"] <- tmp[tmp[,"j"]!=v_j,"j"]
      a_j[tmp$i] <- tmp$x
      #sim_df
      sim_df <- rbind(sim_df,data.frame(ei=i,ej=j,sim=sum(a_i*a_j)/(sum(a_i^2)+sum(a_j^2)-sum(a_i*a_j))))
    }
  }
}
sim_df[order(sim_df$sim,decreasing = T),]
sparseMatrix(i = sim_df$ej,j = sim_df$ei,x = sim_df$sim,dims = c(max(sim_df$ej,sim_df$ei),max(sim_df$ej,sim_df$ei)))
g_edges
