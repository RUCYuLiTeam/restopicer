rm(list = ls(envir = globalenv()))
# not forget setwd("F:/Desktop/restopicer/restopicer-research/NetworkBasedTopicModel")
#####
# required data
#####
load(file = "rdata/research2013.RData")
#load(file = "rdata/research20year.RData")
# if no data, pls run
# source("code/research/researchDataFetch.R")
#####
# required methods
#####
source("code/methods.R")
plotPath="output/research"
addPersistentObjects("plotPath")
#####
# experiment BEGIN
# experiment 1:abstract_bi_lda
# method:LDA
# parameter:
## datatype: abstract
## K: 10
## LDA_method: Gibbs/VEM
#####
rmTempObject()
result_A <- topicDiscovery.LDA(data = researchPapers,datatype = "abstract",K = 10,LDA_method = "Gibbs",plotPath,plotReport = F,papers_tags_df = researchPapersSubjectCategory)
save(result_A,file = "rdata/result_A.RData")
#####
# experiment 2:keywords_bi_lda
# method:LDA
# parameter:
## datatype: keywords
## K: 10
## LDA_method: Gibbs/VEM
#####
rmTempObject()
result_B <- topicDiscovery.LDA(data = researchPapersKeywords,datatype = "keywords",K = 10,LDA_method = "Gibbs",plotPath,plotReport = F,papers_tags_df = researchPapersSubjectCategory)
save(result_B,file = "rdata/result_B.RData")
#####
# experiment 3:keywords_co_fastgreedy
# method:fastgreedy
# p.s. Maximum Spanning Tree
# parameter:
## datatype: keywords
## network_backbone_extract: FALSE -> MST_Threshold: 0
## topic_term_weight: binary/degree
## doc_topic_method: similarity.cos
#####
rmTempObject()
result_C <- topicDiscovery.fastgreedy(data = researchPapersKeywords,datatype = "keywords",MST_Threshold = 0,topic_term_weight = "degree",doc_topic_method = "similarity.cos",plotPath,plotReport = F,papers_tags_df = researchPapersSubjectCategory)
save(result_C,file = "rdata/result_C.RData")
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
rmTempObject()
result_D <- topicDiscovery.linkcomm(data = researchPapersKeywords,datatype = "keywords",MST_Threshold = 0,topic_term_weight = "degree",doc_topic_method = "similarity.cos",plotPath,plotReport = F,papers_tags_df = researchPapersSubjectCategory,link_similarity_method="original")
save(result_D,file = "rdata/result_D.RData")
#####
# *
# experiment 5:keywords_bi_linkcomm
# method:bi_linkcomm
# parameter:
## datatype: keywords
## weight: degree
#####
rmTempObject()
result_E <- topicDiscovery.linkcomm.bipartite(data = researchPapersKeywords,datatype = "keywords",weight = "degree",plotPath = plotPath,plotReport = F,papers_tags_df = researchPapersSubjectCategory,link_similarity_method="original")
save(result_E,file = "rdata/result_E.RData")
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