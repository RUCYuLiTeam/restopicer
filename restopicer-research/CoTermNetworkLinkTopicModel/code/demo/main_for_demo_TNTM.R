rm(list = ls(envir = globalenv()))
setwd("F:/Desktop/restopicer/restopicer-research/CoTermNetworkLinkTopicModel")
#####
# required library
#####
load(file = "rdata/demo.RData")
source(file = "code/functions.R")
##############
# Traditional Network Topic Model demo
##############
# preprocessing
data <- unique(demoPapersKeywords)
bi_matrix <- as.matrix(table(data$item_ut,tolower(data$author_keyword)))
# bipartite network max compart
bi_MaxCompart <- runMaxCompartOfBipartite(bi_matrix)
# new corpus_dtm is the bi_MaxCompart
bi_net <- as.tnet(t(bi_MaxCompart), type = "weighted two-mode tnet")
bi_MaxCompart <- runBipartiteProjecting(t(bi_MaxCompart))


# plot report
plotDocumentTermReport(filename = "demo_LDA_keyword",TF_data = corpus_dtm,plotDocComparison = TRUE,plotTermDist = TRUE, path = "output/demo_LDA_keyword")
plotTopicTermReport(filename = "demo_LDA_keyword",data = topic_posterior$terms,plotTopicComparison = TRUE, plotTopicDist = TRUE, path = "output/demo_LDA_keyword")
plotDocTopicReport(filename = "demo_LDA_keyword",data = topic_posterior$topics,path = "output/demo_LDA_keyword")
##############
# END TNTM demo
##############
