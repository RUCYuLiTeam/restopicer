#####
# method:LDA
# parameter:
## datatype: keywords
## K: best 10 (we test 10,20,30,50,100,150)
## LDA_method: Gibbs
#####
rmTempObject()
result_LDA_keywords_gibbs <- topicDiscovery.LDA(data = researchPapersKeywords,datatype = "keywords",K = 10,LDA_method = "Gibbs",plotPath,plotReport = F,papers_tags_df = researchPapersSubjectCategory)
result_LDA_keywords_VEM <- topicDiscovery.LDA(data = researchPapersKeywords,datatype = "keywords",K = 10,LDA_method = "VEM",plotPath,plotReport = F,papers_tags_df = researchPapersSubjectCategory)
save(result_LDA_keywords_gibbs,file = "rdata/result_LDA_keywords_gibbs.RData")
save(result_LDA_keywords_VEM,file = "rdata/result_LDA_keywords_VEM.RData")
### LDA Keywords
load("rdata/result_LDA_keywords_gibbs.RData")
# doc_topic.taggingtest
result <- result_LDA_keywords_gibbs
doc_topic <- result$doc_topic
papers_tags_df <- researchPapersSubjectCategory
parameter <- result$model$parameter
doc_topic.taggingtest(doc_topic,papers_tags_df,filename = parameter,path = paste(plotPath,parameter,"LeaveOneOut",sep = "/"),LeaveOneOut = T)
#####
# END
#####