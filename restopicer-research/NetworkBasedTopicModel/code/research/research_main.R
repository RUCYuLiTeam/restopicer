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
#####
### LDA abstarct
### LDA Keywords
### fastgreedy keywords
### co_linkcomm keywords


#####
# method:co_linkcomm.percolation
# parameter:
## datatype: keywords
## network_backbone_extract(MST_Threshold method): FULL
## topic_term_weight: degree
## doc_topic_method: similarity.cos
#####
rmTempObject()
result_E <- topicDiscovery.linkcomm.percolation(data = researchPapersKeywords,datatype = "keywords",MST_Threshold = 0,percolation_threshold=0.1,topic_term_weight = "degree",doc_topic_method = "similarity.cos",plotPath,plotReport = F,papers_tags_df = researchPapersSubjectCategory,link_similarity_method="original")
save(result_E,file = "rdata/result_E.RData")
#####
# method:bi_linkcomm
# parameter:
## datatype: keywords
## weight: degree
#####
rmTempObject()
result_G <- topicDiscovery.linkcomm.bipartite(data = researchPapersKeywords,datatype = "keywords",weight = "degree",plotPath = plotPath,plotReport = F,papers_tags_df = researchPapersSubjectCategory,link_similarity_method="original")
save(result_G,file = "rdata/result_G.RData")
#####
# method:co_linkcomm
# parameter:
## datatype: keywords
## link similarity method: ?
## topic_term_weight: degree
## doc_topic_method: similarity.cos
#####

#####
# *
# experiment 7:keywords_bi_linkcomm_?
# method:bi_linkcomm
# parameter:
## datatype: keywords
## link similarity method: ?
## topic_term_weight: degree
## doc_topic_weight: degree
#####

#####
# experiment END
#####