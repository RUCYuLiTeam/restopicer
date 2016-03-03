rm(list = ls(envir = globalenv()))
# not forget setwd("F:/Desktop/restopicer/restopicer-research/NetworkBasedTopicModel")
#####
# required data
#####
#load(file = "rdata/research_2011_2013.RData")
#load(file = "rdata/research_20Y_1994_2013.RData")
# if no data, pls run
# source("code/research/researchDataFetch.R")
#####
# required methods
#####
source("code/methods.R")
foldername <- "research_5yr"
plotPath=paste("output",foldername,sep="/")
addPersistentObjects("plotPath")
addPersistentObjects("foldername")
#####
# method:LDA
# parameter:
## datatype: abstract
## K: best 50 (we test from = 10,to = 100,by = 10)
## LDA_method: Gibbs/VEM
#####
rmTempObject()
k <- 50
#result0 <- NULL
#for(k in seq(from = 10,to = 100,by = 10)){
  result <- topicDiscovery.LDA(data = researchPapers,datatype = "abstract",K = k,LDA_method = "VEM",plotPath,plotReport = F,papers_tags_df = researchPapersSubjectCategory)
#  if(is.null(result0)||(result$model$perplexity<=result0$model$perplexity)){
#    result0 <- result
#  }
#}
result_LDA_abstarct_gibbs <- result
save(result_LDA_abstarct_gibbs,file = paste("rdata",foldername,"result_LDA_abstarct_gibbs.RData",sep="/"))
# analysis
load(paste("rdata",foldername,"result_LDA_abstarct_gibbs.RData",sep="/"))
# doc_topic.taggingtest
result <- result_LDA_abstarct_gibbs
doc_topic <- result$doc_topic
papers_tags_df <- researchPapersSubjectCategory
parameter <- result$model$parameter
doc_topic.taggingtest(doc_topic,papers_tags_df,filename = parameter,path = paste(plotPath,parameter,"LeaveOneOut",sep = "/"),LeaveOneOut = T)
#####
# END
#####
