
#####
# experiment BEGIN
#####
### LDA abstarct
### LDA Keywords
### fastgreedy keywords
### co_linkcomm keywords

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