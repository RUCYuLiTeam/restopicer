#####
# method:co_linkcomm
# parameter:
## datatype: keywords
## topic_term_weight: degree/betweenness/closeness/evcent
## doc_topic_method: cos/ginv
#####
rmTempObject()
result_D <- topicDiscovery.linkcomm(data = researchPapersKeywords,datatype = "keywords",MST_Threshold = 0,cutat = NULL,topic_term_weight = "degree",doc_topic_method = "similarity.cos",plotPath,plotReport = F,papers_tags_df = researchPapersSubjectCategory,link_similarity_method="original")
save(result_D,file = "rdata/result_D.RData")
#####
# END
#####