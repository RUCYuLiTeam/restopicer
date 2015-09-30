rm(list = ls(envir = globalenv()))
# not forget setwd("F:/Desktop/restopicer/restopicer-research/NetworkBasedTopicModel")
#####
# required data
#####
load(file = "rdata/research_20Y_1994_2013.RData")
# if no data, pls run
# source("code/research/researchDataFetch.R")
#####
# required methods
#####
source("code/methods.R")
foldername <- "research_20Y"
plotPath=paste("output",foldername,sep="/")
addPersistentObjects("plotPath")
addPersistentObjects("foldername")
#model train by 2013
#load("rdata/research2013/result_linkcomm_th=0.15_percolation.RData")
# increment get data year by year
# for(year in 1994:2013){
#   fetchdata(year,year)
# }
# increment get model year by year
# th=1
# year=2013
# #http://lijiwei19850620.blog.163.com/blog/static/9784153820136309533661/
# load(paste("rdata/research_",year,"_",year,".RData",sep = ""))
# addPersistentObjects("th")
# addPersistentObjects("year")
# file.remove("rdata/tmp/lc.RData")
# file.remove("rdata/tmp/lc_percolation_sim.RData")
# foldername <- paste("research",year,sep = "_")
# plotPath=paste("output",foldername,sep="/")
# result_linkcomm.percolation_evcent_cos <- topicDiscovery.linkcomm.percolation(data = researchPapersKeywords,datatype = "keywords",MST_Threshold = 0,percolation_threshold=th,cutat = NULL,topic_term_weight = "evcent",doc_topic_method = "similarity.cos",plotPath,plotReport = F,papers_tags_df = researchPapersSubjectCategory,link_similarity_method="original")
# save(result_linkcomm.percolation_evcent_cos,file = paste("rdata/research_20Ys/result_",year,".RData",sep=""))

corpus_dtm <- preprocess.keywords.corpus(researchPapersKeywords)
bi_graph <- graph_from_incidence_matrix(corpus_dtm)
proj_graph <- bipartite_projection(bi_graph, types = NULL, multiplicity = TRUE,probe1 = NULL, which = "true", remove.type = TRUE)
coterm_graph <- simplify(proj_graph)
load(paste("rdata/research_20Ys/result_",2013,".RData",sep=""))
base_community_member_list <- result_linkcomm.percolation_evcent_cos$community_member_list
for(year in 2012:1994){
  load(paste("rdata/research_20Ys/result_",year,".RData",sep=""))
  new_community_member_list <- result_linkcomm.percolation_evcent_cos$community_member_list
  len_base <- length(base_community_member_list)
  len_new <- length(new_community_member_list)
  community_member_list <- c()
  used_comm <- c()
  left_comm_i <- c()
  left_comm_j <- c()
  for(i in 1:len_base){
    comm_pair_df <- data.frame()
    comm_i <- base_community_member_list[[i]]
    #g_i <- delete.vertices(coterm_graph,v = comm_i)
    #k_core_i <- coreness(g_i)
    for(j in 1:len_new){
      comm_j <- new_community_member_list[[j]]
      #g_j <- delete.vertices(coterm_graph,v = comm_j)
      #k_core_j <- coreness(g_j)
      num_support <- length(intersect(comm_i,comm_j))
      num_anti_i <- length(setdiff(comm_i,comm_j))
      num_anti_j <- length(setdiff(comm_j,comm_i))
      sim=ifelse(num_support!=0,num_support/num_anti_j,sqrt(abs(num_anti_i-num_anti_j))/quadWeightImpact(num_anti_i+num_anti_j))
      #sim=mean(distances(coterm_graph,names(k_core_j)[which.max(k_core_j)],names(k_core_i)[which.max(k_core_i)],weights = 1/E(coterm_graph)$weight))
      if(sim>0)comm_pair_df <- rbind(comm_pair_df,data.frame(i,j,sim))
    }
    #print(nrow(comm_pair_df))
    if(nrow(comm_pair_df)!=0){
      for(k in unique(c(comm_pair_df[comm_pair_df$sim>=1,"j"],comm_pair_df[which.max(comm_pair_df$sim)[1],"j"]))){
        community_member_list <- c(community_member_list,list(unique(c(comm_i,new_community_member_list[[k]]))))
        used_comm <- c(used_comm,k)
      }
    }else{
      left_comm_i <- base_community_member_list[i]
    }
  }
  used_comm <- unique(used_comm)
  left_comm_j <- new_community_member_list[-used_comm]
  if(length(left_comm_j)>0){
    for(j in 1:length(left_comm_j)){
      comm_pair_df <- data.frame()
      comm_j <- left_comm_j[[j]]
      g_j <- delete.vertices(coterm_graph,v = comm_j)
      k_core_j <- coreness(g_j)
      for(i in 1:length(community_member_list)){
        comm_i <- community_member_list[[i]]
        g_i <- delete.vertices(coterm_graph,v = comm_i)
        k_core_i <- coreness(g_i)
        dist=mean(distances(coterm_graph,names(k_core_j)[which.max(k_core_j)],names(k_core_i)[which.max(k_core_i)],weights = 1/E(coterm_graph)$weight))/abs(length(comm_j)-length(comm_i))
        comm_pair_df <- rbind(comm_pair_df,data.frame(i,j,dist))
      }
      i <- comm_pair_df[which.min(comm_pair_df$dist)[1],"i"]
      community_member_list[[i]] <- unique(c(community_member_list[[i]],left_comm_j[[j]]))
    }
  }
  base_community_member_list <- community_member_list
}
community_member_list
unlist(lapply(base_community_member_list,FUN = length))
unlist(lapply(community_member_list,FUN = length))
summary(unlist(lapply(community_member_list,FUN = length)))
