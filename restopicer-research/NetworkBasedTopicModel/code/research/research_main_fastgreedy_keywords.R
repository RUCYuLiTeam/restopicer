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
foldername <- "research_2011_2013"
plotPath=paste("output",foldername,sep="/")
addPersistentObjects("plotPath")
addPersistentObjects("foldername")
#####
# method:fastgreedy-no-backbone
# parameter:
## datatype: keywords
## K: 0,according to LDA(20,40)
## topic_term_weight: evcent
## doc_topic_method: cos/ginv
#####
rmTempObject()
for(k in c(0,20,40)){
  result_fastgreedy_evcent_cos <- topicDiscovery.fastgreedy(data = researchPapersKeywords,datatype = "keywords",MST_Threshold = 0,K=k,topic_term_weight = "evcent",doc_topic_method = "similarity.cos",plotPath,plotReport = F,papers_tags_df = researchPapersSubjectCategory)
  save(result_fastgreedy_evcent_cos,file = paste("rdata/",foldername,"/result_fastgreedy_",k,".RData",sep=""))
}
#####
# END
#####