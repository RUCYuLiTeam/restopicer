rm(list = ls(envir = globalenv()))
setwd("F:/Desktop/restopicer/restopicer-research/LinkTopicModel")
#####
# required library
#####
source(file = "code/functions.R")
load(file = "rdata/demo.RData")
##############
# Traditional Network Topic Model demo
##############
# preprocessing
data <- unique(demoPapersKeywords)
bi_matrix <- table(data$item_ut,tolower(data$author_keyword))
# bipartite network max compart
# bi_compart <- compart(bi_matrix)
# bi_compart$n.compart
# size.compart <- data.frame(doc=row.names(bi_compart$cweb),compart=-apply(bi_compart$cweb,1,FUN = min)) %>% group_by(compart) %>% summarise(cnt = n())
# max.size.compart <- size.compart[which.max(size.compart$cnt),]$compart
# doc.compart <- data.frame(doc=row.names(bi_compart$cweb),compart=-apply(bi_compart$cweb,1,FUN = min)) %>% filter(compart == max.size.compart)
# bi_MaxCompart <- bi_matrix[row.names(bi_matrix) %in% doc.compart$doc,]
# bi_MaxCompart[which(bi_MaxCompart!=0)] <- 1
# bi_MaxCompart <- empty(bi_MaxCompart)
# new corpus_dtm is the bi_MaxCompart



# plot report
plotDocumentTermMatrixReport(filename = "demo_LDA_keyword",TF_data = corpus_dtm,plotDocComparison = TRUE,plotTermDist = TRUE, path = "output/demo_LDA_keyword")
plotTopicTermMatrixReport(filename = "demo_LDA_keyword",data = topic_posterior$terms,plotTopicComparison = TRUE, plotTopicDist = TRUE, path = "output/demo_LDA_keyword")
plotDocTopicMatrixReport(filename = "demo_LDA_keyword",data = topic_posterior$topics,path = "output/demo_LDA_keyword")
##############
# END TNTM demo
##############
