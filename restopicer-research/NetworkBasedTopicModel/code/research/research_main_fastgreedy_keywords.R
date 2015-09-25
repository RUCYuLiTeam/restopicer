rm(list = ls(envir = globalenv()))
# not forget setwd("F:/Desktop/restopicer/restopicer-research/NetworkBasedTopicModel")
#####
# required data
#####
load(file = "rdata/research2013.RData")
#load(file = "rdata/research_20Y_1994_2013.RData")
# if no data, pls run
# source("code/research/researchDataFetch.R")
#####
# required methods
#####
source("code/methods.R")
foldername <- "research2013"
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
  result_fastgreedy_evcent_ginv <- topicDiscovery.fastgreedy(data = researchPapersKeywords,datatype = "keywords",MST_Threshold = 0,K=k,topic_term_weight = "evcent",doc_topic_method = "Moore-Penrose",plotPath,plotReport = F,papers_tags_df = researchPapersSubjectCategory)
  save(result_fastgreedy_evcent_cos,result_fastgreedy_evcent_ginv,file = paste("rdata/",foldername,"/result_fastgreedy_",k,".RData",sep=""))
  result_list <- list(result_fastgreedy_evcent_cos,result_fastgreedy_evcent_ginv)
  # doc_topic.taggingtest
  for(result in result_list){
    doc_topic <- result$doc_topic
    papers_tags_df <- researchPapersSubjectCategory
    parameter <- result$model$parameter
    doc_topic.taggingtest(doc_topic,papers_tags_df,filename = parameter,path = paste(plotPath,parameter,"LeaveOneOut",sep = "/"),LeaveOneOut = T)
  }
}
#####
# END
#####