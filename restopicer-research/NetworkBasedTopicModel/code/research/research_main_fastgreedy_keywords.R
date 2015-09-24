#####
# method:fastgreedy-no-backbone
# parameter:
## datatype: keywords
## topic_term_weight: degree/betweenness/closeness/evcent
## doc_topic_method: cos/ginv
#####
rmTempObject()
result_fastgreedy_degree_cos <- topicDiscovery.fastgreedy(data = researchPapersKeywords,datatype = "keywords",MST_Threshold = 0,K=0,topic_term_weight = "degree",doc_topic_method = "similarity.cos",plotPath,plotReport = F,papers_tags_df = researchPapersSubjectCategory)
result_fastgreedy_degree_ginv <- topicDiscovery.fastgreedy(data = researchPapersKeywords,datatype = "keywords",MST_Threshold = 0,K=0,topic_term_weight = "degree",doc_topic_method = "Moore-Penrose",plotPath,plotReport = F,papers_tags_df = researchPapersSubjectCategory)
#result_fastgreedy_betweenness_cos <- topicDiscovery.fastgreedy(data = researchPapersKeywords,datatype = "keywords",MST_Threshold = 0,K=0,topic_term_weight = "betweenness",doc_topic_method = "similarity.cos",plotPath,plotReport = F,papers_tags_df = researchPapersSubjectCategory)
#result_fastgreedy_betweenness_ginv <- topicDiscovery.fastgreedy(data = researchPapersKeywords,datatype = "keywords",MST_Threshold = 0,K=0,topic_term_weight = "betweenness",doc_topic_method = "Moore-Penrose",plotPath,plotReport = F,papers_tags_df = researchPapersSubjectCategory)
result_fastgreedy_closeness_cos <- topicDiscovery.fastgreedy(data = researchPapersKeywords,datatype = "keywords",MST_Threshold = 0,K=0,topic_term_weight = "closeness",doc_topic_method = "similarity.cos",plotPath,plotReport = F,papers_tags_df = researchPapersSubjectCategory)
result_fastgreedy_closeness_ginv <- topicDiscovery.fastgreedy(data = researchPapersKeywords,datatype = "keywords",MST_Threshold = 0,K=0,topic_term_weight = "closeness",doc_topic_method = "Moore-Penrose",plotPath,plotReport = F,papers_tags_df = researchPapersSubjectCategory)
result_fastgreedy_evcent_cos <- topicDiscovery.fastgreedy(data = researchPapersKeywords,datatype = "keywords",MST_Threshold = 0,K=0,topic_term_weight = "evcent",doc_topic_method = "similarity.cos",plotPath,plotReport = F,papers_tags_df = researchPapersSubjectCategory)
result_fastgreedy_evcent_ginv <- topicDiscovery.fastgreedy(data = researchPapersKeywords,datatype = "keywords",MST_Threshold = 0,K=0,topic_term_weight = "evcent",doc_topic_method = "Moore-Penrose",plotPath,plotReport = F,papers_tags_df = researchPapersSubjectCategory)
save(result_fastgreedy_degree_cos,result_fastgreedy_degree_ginv,
     #result_fastgreedy_betweenness_cos,result_fastgreedy_betweenness_ginv,
     result_fastgreedy_closeness_cos,result_fastgreedy_closeness_ginv,
     result_fastgreedy_evcent_cos,result_fastgreedy_evcent_ginv,
     file = "rdata/result_fastgreedy.RData")
### fastgreedy
load("rdata/result_fastgreedy.RData")
# doc_topic.taggingtest
result_list <- list(result_fastgreedy_degree_cos,result_fastgreedy_degree_ginv,
     #result_fastgreedy_betweenness_cos,result_fastgreedy_betweenness_ginv,
     result_fastgreedy_closeness_cos,result_fastgreedy_closeness_ginv,
     result_fastgreedy_evcent_cos,result_fastgreedy_evcent_ginv)
for(result in result_list){
  doc_topic <- result$doc_topic
  papers_tags_df <- researchPapersSubjectCategory
  parameter <- result$model$parameter
  doc_topic.taggingtest(doc_topic,papers_tags_df,filename = parameter,path = paste(plotPath,parameter,"LeaveOneOut",sep = "/"),LeaveOneOut = T)
}
#####
# END
#####