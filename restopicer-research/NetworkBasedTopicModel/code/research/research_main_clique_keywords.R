rm(list = ls(envir = globalenv()))
# not forget setwd("F:/Desktop/restopicer/restopicer-research/NetworkBasedTopicModel")
#####
# required data
#####
load(file = "rdata/research_3Y_2011_2013.RData")
# if no data, pls run
# fetchdata(from_year = 2009,to_year = 2013)
#####
# required methods
#####
source("code/methods.R")
foldername <- "research_2011_2013_clique"
plotPath=paste("output",foldername,sep="/")
addPersistentObjects("plotPath")
addPersistentObjects("foldername")

# method:co_linkcomm.percolation
# parameter:
## datatype: keywords
## percolation_threshold: 
## topic_term_weight: evcent
## doc_topic_method: cos/ginv
#####
rmTempObject()
result_clique.percolation_evcent_cos <- topicDiscovery.clique.percolation(data = researchPapersKeywords,datatype = "keywords",MST_Threshold = 0,clique_k=30,topic_term_weight = "evcent",doc_topic_method = "similarity.cos",plotPath,plotReport = F,papers_tags_df = researchPapersSubjectCategory)
save(result_clique.percolation_evcent_cos,
       file = paste("rdata/",foldername,"/result_clique_percolation.RData",sep=""))

