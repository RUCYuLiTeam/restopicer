rm(list = ls(envir = globalenv()))
setwd("F:/Desktop/restopicer/restopicer-research/LinkTopicModel")
#####
# required library
#####
library(topicmodels)
library(bipartite)
library(dplyr)
source(file = "code/functions.R")
load(file = "rdata/demo.RData")
##############
# LDA on abstract demo
##############
# preprocessing
data <- unique(demoPapers)
corpus <- VCorpus(VectorSource(data$abstract))
l_ply(.data = 1:length(corpus),.fun = function(i){
  meta(corpus[[i]],tag = "id") <<- data$item_ut[i]
  meta(corpus[[i]],tag = "title") <<- data$article_title[i]
  meta(corpus[[i]],tag = "cited_count") <<- data$cited_count[i]
  meta(corpus[[i]],tag = "document_type") <<- data$document_type[i]
  meta(corpus[[i]],tag = "publisher") <<- data$full_source_title[i]
  meta(corpus[[i]],tag = "publication_type") <<- data$publication_type[i]
  meta(corpus[[i]],tag = "year") <<- data$publication_year[i]
})
# fpattern <- content_transformer(function(x, pattern) gsub(pattern, "", x))
# complainCorpus <- tm_map(complainCorpus, fpattern, "z*")
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, stopwords("SMART"))
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stripWhitespace)
strsplit_space_tokenizer <- function(x){
  unlist(strsplit(as.character(x), "[[:space:]]+"))
}
# control <- list(weighting = weightTf, tokenize= strsplit_space_tokenizer,
#                 tolower = TRUE, removePunctuation = TRUE, removeNumbers = TRUE, stopwords = TRUE, stemming = TRUE,
#                 dictionary = NULL, bounds = list(local = c(1, Inf)), wordLengths = c(3, Inf))
control <- list(weighting = weightTf, tokenize= words,
                tolower = TRUE, removePunctuation = TRUE, removeNumbers = TRUE, stopwords = TRUE, stemming = FALSE,
                dictionary = NULL, bounds = list(local = c(1, Inf)), wordLengths = c(3, Inf))
corpus_dtm <- DocumentTermMatrix(corpus,control)
# LDA run
k <- 10
SEED <- 2015
corpus_topic <- list(VEM = LDA(corpus_dtm, k = k, control = list(seed = SEED)),
                     Gibbs = LDA(corpus_dtm, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000)))
#熵值越高说明主题分布更均匀
sapply(corpus_topic,  function(x)  mean(apply(posterior(x)$topics,1,  function(z) -sum(z*log(z)))))
topic_posterior <- posterior(corpus_topic[["Gibbs"]])
# plot report
plotDocumentTermMatrixReport(filename = "demo_LDA_abstract",TF_data = corpus_dtm,plotDocComparison = TRUE,plotTermDist = TRUE, path = "output/demo_LDA_abstract")
plotTopicTermMatrixReport(filename = "demo_LDA_abstract",data = topic_posterior$terms,plotTopicComparison = TRUE, plotTopicDist = TRUE, path = "output/demo_LDA_abstract")
plotDocTopicMatrixReport(filename = "demo_LDA_abstract",data = topic_posterior$topics,path = "output/demo_LDA_abstract")
##############
# LDA on keywords demo
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
corpus_dtm <- as.DocumentTermMatrix(bi_matrix,weighting = weightTf)
# LDA run
k <- 10
SEED <- 2015
corpus_topic <- list(VEM = LDA(corpus_dtm, k = k, control = list(seed = SEED)),
                     Gibbs = LDA(corpus_dtm, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000)))
#熵值越高说明主题分布更均匀
sapply(corpus_topic,  function(x)  mean(apply(posterior(x)$topics,1,  function(z) -sum(z*log(z)))))
topic_posterior <- posterior(corpus_topic[["Gibbs"]])
# plot report
plotDocumentTermMatrixReport(filename = "demo_LDA_keyword",TF_data = corpus_dtm,plotDocComparison = TRUE,plotTermDist = TRUE, path = "output/demo_LDA_keyword")
plotTopicTermMatrixReport(filename = "demo_LDA_keyword",data = topic_posterior$terms,plotTopicComparison = TRUE, plotTopicDist = TRUE, path = "output/demo_LDA_keyword")
plotDocTopicMatrixReport(filename = "demo_LDA_keyword",data = topic_posterior$topics,path = "output/demo_LDA_keyword")
##############
# END LDA on demo
##############