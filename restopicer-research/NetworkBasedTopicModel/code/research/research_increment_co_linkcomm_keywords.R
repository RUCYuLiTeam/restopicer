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
th=1
year=2013
#http://lijiwei19850620.blog.163.com/blog/static/9784153820136309533661/
load(paste("rdata/research_",year,"_",year,".RData",sep = ""))
addPersistentObjects("th")
addPersistentObjects("year")
file.remove("rdata/tmp/lc.RData")
file.remove("rdata/tmp/lc_percolation_sim.RData")
foldername <- paste("research",year,sep = "_")
plotPath=paste("output",foldername,sep="/")
result_linkcomm.percolation_evcent_cos <- topicDiscovery.linkcomm.percolation(data = researchPapersKeywords,datatype = "keywords",MST_Threshold = 0,percolation_threshold=th,cutat = NULL,topic_term_weight = "evcent",doc_topic_method = "similarity.cos",plotPath,plotReport = F,papers_tags_df = researchPapersSubjectCategory,link_similarity_method="original")
save(result_linkcomm.percolation_evcent_cos,file = paste("rdata/research_20Ys/result_",year,".RData",sep=""))

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
  base_continue_comm <- c()
  #print(year)
  for(i in 1:len_new){
    comm_pair_df <- data.frame()
    comm_i <- new_community_member_list[[i]]
    #g_i <- delete.vertices(coterm_graph,v = comm_i)
    #k_core_i <- coreness(g_i)
    for(j in 1:len_base){
      comm_j <- base_community_member_list[[j]]
      #g_j <- delete.vertices(coterm_graph,v = comm_j)
      #k_core_j <- coreness(g_j)
      num_support <- length(intersect(comm_i,comm_j))
      #num_anti_i <- length(setdiff(comm_i,comm_j))
      num_anti_j <- length(setdiff(comm_j,comm_i))
      sim=num_support/quadWeightImpact(num_anti_j)
      #sim=mean(distances(coterm_graph,names(k_core_j)[which.max(k_core_j)],names(k_core_i)[which.max(k_core_i)],weights = 1/E(coterm_graph)$weight))
      if(sim>0)comm_pair_df <- rbind(comm_pair_df,data.frame(i,j,sim))
    }
    #print(nrow(comm_pair_df))
    if(nrow(comm_pair_df)!=0){
      for(k in comm_pair_df[comm_pair_df$sim>1.5,"j"]){
        community_member_list <- c(community_member_list,list(unique(new_community_member_list[[i]],base_community_member_list[[k]])))
        base_continue_comm <- c(base_continue_comm,k)
      }
    }else{
      community_member_list <- c(community_member_list,new_community_member_list[i])
    }
  }
  base_continue_comm <- unique(base_continue_comm)
  if(!is.null(base_continue_comm)){
    base_community_member_list <- c(community_member_list,base_community_member_list[-(base_continue_comm)])
  }else{
    base_community_member_list <- c(community_member_list,base_community_member_list)
  }
}
community_member_list <- base_community_member_list
unlist(lapply(community_member_list,FUN = length))
summary(unlist(lapply(community_member_list,FUN = length)))
