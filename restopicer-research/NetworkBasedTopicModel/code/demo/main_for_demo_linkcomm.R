rm(list = ls(envir = globalenv()))
setwd("F:/Desktop/restopicer/restopicer-research/NetworkBasedTopicModel")
#####
# required library
#####
library(linkcomm)
load(file = "rdata/demo.RData")
source(file = "code/functions.R")
##############
# Traditional Network Topic Model demo
# linkcomm.community
##############
# preprocessing
data <- unique(demoPapersKeywords)
bi_matrix <- table(data$item_ut,tolower(data$author_keyword))
# bipartite network max compart
#bi_MaxCompart <- runMaxCompartOfMatrix(bi_matrix)
bi_MaxCompart <- bi_matrix
# bipartite from incidence matrix
bi_g <- graph_from_incidence_matrix(bi_MaxCompart)
# projecting of two side
proj_g <- bipartite_projection(bi_g, types = NULL, multiplicity = TRUE,probe1 = NULL, which = "both", remove.type = TRUE)
# run linkcomm community
coterm_g <- proj_g[[2]]
coterm_g <- simplify(coterm_g)
coterm_edgelist <- as.data.frame(cbind(get.edgelist(coterm_g),get.edge.attribute(coterm_g,name = "weight")),stringsAsFactors = F)
coterm_edgelist$V3 <- as.numeric(coterm_edgelist$V3)
lc <- getLinkCommunities(coterm_edgelist,hcmethod="average",bipartite=F,dist = NULL)
community_member_list <- lapply(split(lc$nodeclusters$node,f = lc$nodeclusters$cluster),FUN = function(x){unlist(as.character(x))})
# get the term-term matrix
# coterm_g_matrix <- as_adjacency_matrix(coterm_g,type = "both",attr="weight")
# generate topic-term matrix through community
topic_term <- getTopicMemberBipartiteMatrix(community_member_list,weight = "binary")
# calculate similarity to get doc-topic matrix
doc_topic <- getDocTopicBipartiteMatrix(doc_member = bi_MaxCompart,topic_member = topic_term,method = "similarity.cos")
# document tagging test
taggingtest_doc_topic <- cbind(item_ut=rownames(doc_topic),as.data.frame(doc_topic))
taggingtest_doc_sc <- unique(demoPapersSubjectCategory[,c("item_ut","subject_category")])
taggingtest_data <- merge(taggingtest_doc_topic, taggingtest_doc_sc)
# plot report
doc.tagging.test(taggingtest_data = taggingtest_data,filename = "demo_linkcomm_keyword",path = "output/demo_linkcomm_keyword/document_topic",LeaveOneOut = FALSE)
# network of topic
plotTopicNetworkReport(filename = "demo_linkcomm_keyword",graph = coterm_g,community_member_list,showNamesInPlot = TRUE,plotCommunity = TRUE,plotOverallTopics = TRUE,path = "output/demo_linkcomm_keyword/topic_term")
# transpose = FALSE
plotBipartiteMatrixReport(filename = "demo_linkcomm_keyword",bi_matrix = bi_MaxCompart,path = "output/demo_linkcomm_keyword/document_term",showNamesInPlot = FALSE, weightType = "tfidf", plotRowWordCloud = TRUE, plotWordCloud = TRUE, plotRowComparison = TRUE, plotRowDist = TRUE, plotModules = FALSE)
plotBipartiteMatrixReport(filename = "demo_linkcomm_keyword",bi_matrix = topic_term,path = "output/demo_linkcomm_keyword/topic_term",showNamesInPlot = FALSE, weightType = "tf", plotRowWordCloud = TRUE, plotWordCloud = TRUE, plotRowComparison = TRUE, plotRowDist = TRUE, plotModules = FALSE)
plotBipartiteMatrixReport(filename = "demo_linkcomm_keyword",bi_matrix = doc_topic,path = "output/demo_linkcomm_keyword/document_topic",showNamesInPlot = FALSE, weightType = "tf", plotRowWordCloud = TRUE, plotWordCloud = TRUE, plotRowComparison = TRUE, plotRowDist = TRUE, plotModules = FALSE)
# transpose = TRUE
plotBipartiteMatrixReport(filename = "demo_linkcomm_keyword",bi_matrix = bi_MaxCompart,transpose = TRUE,path = "output/demo_linkcomm_keyword/document_term",showNamesInPlot = FALSE, weightType = "tfidf", plotRowWordCloud = FALSE, plotWordCloud = FALSE, plotRowComparison = FALSE, plotRowDist = TRUE, plotModules = FALSE)
plotBipartiteMatrixReport(filename = "demo_linkcomm_keyword",bi_matrix = topic_term,transpose = TRUE,path = "output/demo_linkcomm_keyword/topic_term",showNamesInPlot = FALSE, weightType = "tf", plotRowWordCloud = FALSE, plotWordCloud = FALSE, plotRowComparison = FALSE, plotRowDist = TRUE, plotModules = FALSE)
plotBipartiteMatrixReport(filename = "demo_linkcomm_keyword",bi_matrix = doc_topic,transpose = TRUE,path = "output/demo_linkcomm_keyword/document_topic",showNamesInPlot = FALSE, weightType = "tf", plotRowWordCloud = FALSE, plotWordCloud = FALSE, plotRowComparison = FALSE, plotRowDist = TRUE, plotModules = FALSE)
##############
# END TNTM-linkcomm demo
##############
