rm(list = ls(envir = globalenv()))
# not forget setwd("F:/Desktop/restopicer/restopicer-research/NetworkBasedTopicModel")
#####
# required data
#####
load(file = "rdata/research_2011_2013.RData")
# if no data, pls run
# fetchdata(from_year = 2009,to_year = 2013)
#####
# required methods
#####
source("code/methods.R")
foldername <- "research_2011_2013_again"
plotPath=paste("output",foldername,sep="/")
addPersistentObjects("plotPath")
addPersistentObjects("foldername")
#####
# method:topicDiscovery.linkcomm.percolation.edge
# parameter:
## datatype: keywords
## percolation_threshold: th and cut_at
## topic_term_weight: evcent
## doc_topic_method: cos
#####
rmTempObject()
#list(NULL,0.6,0.5,0.4)
for(cutat_th in list(NULL,0.6,0.5,0.4)){
  #seq(from = 0.1,to = 1,by = 0.05)
  #c(0.05,0.1,0.15,0.2,0.25)
  for(th in c(0.1,0.15,0.2,0.25)){
    result_linkcomm.percolation.edge_evcent_cos <- 
      topicDiscovery.linkcomm.percolation.edge(data = researchPapersKeywords,
                                               datatype = "keywords",
                                               MST_Threshold = 0,
                                               percolation_threshold=th,
                                               cutat_th = cutat_th,
                                               topic_term_weight = "evcent",
                                               doc_topic_method = "similarity.cos",
                                               plotPath,plotReport = F,
                                               papers_tags_df = researchPapersSubjectCategory,
                                               link_similarity_method="original")
    save(result_linkcomm.percolation.edge_evcent_cos,
         file = paste("rdata/",foldername,"/cutat_th=",cutat_th,"_th=",th,".RData",sep=""))
  }
}