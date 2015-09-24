#####
# method:LDA
# parameter:
## datatype: abstract
## K: best 50 (we test 10,20,30,50,100,150)
## LDA_method: Gibbs/VEM
#####
rmTempObject()
result_LDA_abstarct_gibbs <- topicDiscovery.LDA(data = researchPapers,datatype = "abstract",K = 50,LDA_method = "Gibbs",plotPath,plotReport = F,papers_tags_df = researchPapersSubjectCategory)
save(result_LDA_abstarct_gibbs,file = "rdata/result_LDA_abstarct_gibbs.RData")
result_LDA_abstarct_VEM <- topicDiscovery.LDA(data = researchPapers,datatype = "abstract",K = 50,LDA_method = "VEM",plotPath,plotReport = F,papers_tags_df = researchPapersSubjectCategory)
save(result_LDA_abstarct_VEM,file = "rdata/result_LDA_abstarct_VEM.RData")
### LDA abstarct
load("rdata/result_LDA_abstarct_gibbs.RData")
# doc_topic.taggingtest
result <- result_LDA_abstarct_gibbs
doc_topic <- result$doc_topic
papers_tags_df <- researchPapersSubjectCategory
parameter <- result$model$parameter
doc_topic.taggingtest(doc_topic,papers_tags_df,filename = parameter,path = paste(plotPath,parameter,"LeaveOneOut",sep = "/"),LeaveOneOut = T)
#####
# END
#####
