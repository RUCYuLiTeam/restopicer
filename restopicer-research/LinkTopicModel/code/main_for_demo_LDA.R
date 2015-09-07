rm(list = ls(envir = globalenv()))
setwd("F:/Desktop/restopicer/restopicer-research/LinkTopicModel")
#####
# required library
#####
library(tm)
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
                tolower = TRUE, removePunctuation = TRUE, removeNumbers = TRUE, stopwords = TRUE, stemming = TRUE,
                dictionary = NULL, bounds = list(local = c(1, Inf)), wordLengths = c(3, Inf))
corpus_dtm <- DocumentTermMatrix(corpus,control)
# wordcloud

##############
# LDA on keywords demo
##############
