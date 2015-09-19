rm(list = ls(envir = globalenv()))
# not forget setwd("F:/Desktop/restopicer/restopicer-research/NetworkBasedTopicModel")
#####
# required data
#####
load(file = "rdata/demo.RData")
# if no data, pls run
# source("code/demo/demoDataFetch.R")
#####
# required methods
#####
source("code/methods.R")
plotPath="output/demo"
#####
# experiment BEGIN
# experiment 1:abstract_bi_lda
# method:LDA
# parameter:
## datatype: abstract
## K: 10
## LDA_method: Gibbs
#####
topicDiscovery.LDA(data = demoPapers,datatype = "abstract",K = 10,LDA_method = "Gibbs",plotPath,plotReport = T,papers_tags_df = demoPapersSubjectCategory)
#####
# experiment 2:keywords_bi_lda
# method:LDA
# parameter:
## datatype: keywords
## K: 10
## LDA_method: Gibbs
#####
topicDiscovery.LDA(data = demoPapersKeywords,datatype = "keywords",K = 10,LDA_method = "Gibbs",plotPath,plotReport = T,papers_tags_df = demoPapersSubjectCategory)
#####
# experiment 3:keywords_co_fastgreedy
# method:fastgreedy
# p.s. Maximum Spanning Tree
# parameter:
## datatype: keywords
## network_backbone_extract(MST_Threshold method): FULL
## topic_term_weight: binary/degree
## doc_topic_method: similarity.cos
#####

#####
# *
# experiment 4:keywords_co_linkcomm
# method:co_linkcomm
# parameter:
## datatype: keywords
## network_backbone_extract(MST_Threshold method): FULL
## topic_term_weight: binary/degree
## doc_topic_method: similarity.cos
#####

#####
# *
# experiment 5:keywords_bi_linkcomm
# method:bi_linkcomm
# parameter:
## datatype: keywords
## topic_term_weight: binary/degree
## doc_topic_weight: binary/degree
#####

#####
# *
# experiment 6:keywords_co_linkcomm_coneighbor/innerlink
# method:co_linkcomm
# parameter:
## datatype: keywords
## link similarity method: coneighbor/innerlink
## topic_term_weight: binary/degree
## doc_topic_method: similarity.cos
#####

#####
# *
# experiment 7:keywords_bi_linkcomm_?
# method:bi_linkcomm
# parameter:
## datatype: keywords
## link similarity method: ?
## topic_term_weight: binary/degree
## doc_topic_weight: binary/degree
#####

#####
# experiment END
#####