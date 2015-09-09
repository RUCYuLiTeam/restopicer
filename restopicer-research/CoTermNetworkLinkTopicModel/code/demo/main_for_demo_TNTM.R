rm(list = ls(envir = globalenv()))
setwd("F:/Desktop/restopicer/restopicer-research/CoTermNetworkLinkTopicModel")
#####
# required library
#####
library(igraph)
load(file = "rdata/demo.RData")
source(file = "code/functions.R")
##############
# Traditional Network Topic Model demo
##############
# preprocessing
data <- unique(demoPapersKeywords)
bi_matrix <- table(data$item_ut,tolower(data$author_keyword))
# bipartite network max compart
bi_MaxCompart <- runMaxCompartOfBipartite(bi_matrix)
# transform from matrix to edgelist
bi_edgelist <- as.tnet(t(bi_MaxCompart), type = "weighted two-mode tnet")
bi_vertice_p <- data.frame(id = 1:nrow(bi_MaxCompart),name = rownames(bi_MaxCompart), stringsAsFactors = F)
bi_vertice_i <- data.frame(id = 1:ncol(bi_MaxCompart),name = colnames(bi_MaxCompart), stringsAsFactors = F)
# run Bipartite Projecting
coterm_edgelist <- runBipartiteProjecting(bi_edgelist)
coterm_vertice <- bi_vertice_i
coterm_edgelist <- merge(coterm_edgelist, cbind(i = coterm_vertice$id, i_name = coterm_vertice$name))
coterm_edgelist <- merge(coterm_edgelist, cbind(j = coterm_vertice$id, j_name = coterm_vertice$name))
# run fastgreedy community
coterm_g <- graph.edgelist(el = as.matrix(coterm_edgelist[,c("i_name","j_name")]),directed = FALSE)
E(coterm_g)$weight <- coterm_edgelist$w
coterm_g <- simplify(coterm_g)
fc <- fastgreedy.community(coterm_g)
modularity(fc)
# generate topic-term matrix through community

# calculate similarity to get doc-topic matrix

# plot report
plotDocumentTermReport(filename = "demo_LDA_keyword",TF_data = corpus_dtm,plotDocComparison = TRUE,plotTermDist = TRUE, path = "output/demo_LDA_keyword")
plotTopicTermReport(filename = "demo_LDA_keyword",data = topic_posterior$terms,plotTopicComparison = TRUE, plotTopicDist = TRUE, path = "output/demo_LDA_keyword")
plotDocTopicReport(filename = "demo_LDA_keyword",data = topic_posterior$topics,path = "output/demo_LDA_keyword")
##############
# END TNTM demo
##############
