rm(list = ls(envir = globalenv()))
setwd("F:/Desktop/restopicer/restopicer-research/CoTermNetworkLinkTopicModel")
#####
# required library
#####
source(file = "code/functions.R")
load(file = "rdata/demo.RData")
##############
# Traditional Network Topic Model demo
##############
# preprocessing
data <- unique(demoPapersKeywords)
bi_matrix <- table(data$item_ut,tolower(data$author_keyword))
# bipartite network max compart

# new corpus_dtm is the bi_MaxCompart
projecting_tm(t(bi_MaxCompart),method = "sum")
projectingKeywordNetwork <- list(keyword=colnames(bi_MaxCompart),coterm=projecting_tm(t(bi_MaxCompart),method = "sum"))



# plot report
plotDocumentTermReport(filename = "demo_LDA_keyword",TF_data = corpus_dtm,plotDocComparison = TRUE,plotTermDist = TRUE, path = "output/demo_LDA_keyword")
plotTopicTermReport(filename = "demo_LDA_keyword",data = topic_posterior$terms,plotTopicComparison = TRUE, plotTopicDist = TRUE, path = "output/demo_LDA_keyword")
plotDocTopicReport(filename = "demo_LDA_keyword",data = topic_posterior$topics,path = "output/demo_LDA_keyword")
##############
# END TNTM demo
##############
