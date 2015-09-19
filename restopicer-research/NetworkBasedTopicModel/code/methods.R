#####
# abstract_bi_lda
# http://yuedu.baidu.com/ebook/d0b441a8ccbff121dd36839a
# http://blog.csdn.net/pirage/article/details/9467547
#####
topicDiscovery.LDA <- function(data,datatype="abstract",
                               K=10,LDA_method="Gibbs",
                               plotPath="output/demo",plotReport=TRUE,papers_tags_df=NULL){
  # step 1:preprocessing corpus
  corpus_dtm <- switch(datatype,
              "abstract" = preprocess.abstract.corpus(data),
              "keywords" = preprocess.keywords.corpus(data))
  # step 2:runing LDA model 
  SEED <- 19910513
  corpus_topic <- switch(LDA_method,
                       "Gibbs" = LDA(corpus_dtm, k = K, method = LDA_method, control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000)),
                       "VEM" = LDA(corpus_dtm, k = K,method = LDA_method, control = list(seed = SEED)))
  # step 3:doc_topic and topic_term matrix
  topic_posterior <- posterior(corpus_topic)
  doc_topic <- topic_posterior$topics
  topic_term <- topic_posterior$terms
  # step 4:plot Report
  if(plotReport){
    model <- NULL
    # filenames and foldername
    model$parameter <- paste(datatype,"LDA",K,LDA_method,sep = "_")
    #perplexity
    model$perplexity <- perplexity(corpus_topic,corpus_dtm)
    #entropy
    model$entropy <- mean(apply(doc_topic,1,function(z) -sum(z*log(z))))
    # folder
    if(!file.exists(file.path(plotPath,model$parameter))) dir.create(file.path(plotPath,model$parameter),recursive = TRUE)
    write.table(as.data.frame(model),file = file.path(plotPath,model$parameter,paste(model$parameter,"modeltest.txt",sep="-")),quote = F,sep = "\t",row.names = F,col.names = T)
    # doc_topic for taggingtest
    doc_topic.taggingtest(doc_topic,papers_tags_df,filename = model$parameter,path = paste(plotPath,model$parameter,sep = "/"),LeaveOneOut = F)
    # matrix plot
    plotReport.bipartite.matrix(corpus_dtm,topic_term,doc_topic,filename = model$parameter,path = paste(plotPath,model$parameter,sep = "/"))
  }
}
#####
# preprocessing method
#####
preprocess.abstract.corpus <- function(papers_df){
  data <- unique(papers_df)
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
  corpus_dtm
}
preprocess.keywords.corpus <- function(papers_keywords_df){
  data <- unique(papers_keywords_df)
  bi_matrix <- table(data$item_ut,tolower(data$author_keyword))
  corpus_dtm <- as.DocumentTermMatrix(bi_matrix,weighting = weightTf)
  corpus_dtm
}
#####
# bipartite matrix plot report
#####
plotReport.bipartite.matrix <- function(corpus_dtm,topic_term,doc_topic,filename,path){
  # transpose = FALSE
  plotBipartiteMatrixReport(filename = filename,bi_matrix = corpus_dtm,path = paste(path,"document_term",sep = "/"),showNamesInPlot = FALSE, weightType = "tfidf", plotRowWordCloud = TRUE, plotWordCloud = TRUE, plotRowComparison = TRUE, plotRowDist = TRUE, plotModules = FALSE)
  plotBipartiteMatrixReport(filename = filename,bi_matrix = topic_term,path = paste(path,"topic_term",sep = "/"),showNamesInPlot = FALSE, weightType = "tf", plotRowWordCloud = TRUE, plotWordCloud = TRUE, plotRowComparison = TRUE, plotRowDist = TRUE, plotModules = FALSE)
  plotBipartiteMatrixReport(filename = filename,bi_matrix = doc_topic,path = paste(path,"doc_term",sep = "/"),showNamesInPlot = FALSE, weightType = "tf", plotRowWordCloud = TRUE, plotWordCloud = TRUE, plotRowComparison = TRUE, plotRowDist = TRUE, plotModules = FALSE)
  # transpose = TRUE
  plotBipartiteMatrixReport(filename = filename,bi_matrix = corpus_dtm,transpose = TRUE,path = paste(path,"document_term",sep = "/"),showNamesInPlot = FALSE, weightType = "tfidf", plotRowWordCloud = FALSE, plotWordCloud = FALSE, plotRowComparison = FALSE, plotRowDist = TRUE, plotModules = FALSE)
  plotBipartiteMatrixReport(filename = filename,bi_matrix = topic_term,transpose = TRUE,path = paste(path,"topic_term",sep = "/"),showNamesInPlot = FALSE, weightType = "tf", plotRowWordCloud = FALSE, plotWordCloud = FALSE, plotRowComparison = FALSE, plotRowDist = TRUE, plotModules = FALSE)
  plotBipartiteMatrixReport(filename = filename,bi_matrix = doc_topic,transpose = TRUE,path = paste(path,"doc_term",sep = "/"),showNamesInPlot = FALSE, weightType = "tf", plotRowWordCloud = FALSE, plotWordCloud = FALSE, plotRowComparison = FALSE, plotRowDist = TRUE, plotModules = FALSE)
}
#####
# document tagging test for doc_topic
#####
doc_topic.taggingtest <- function(doc_topic,papers_tags_df,filename,path,LeaveOneOut = FALSE){
  taggingtest_doc_topic <- cbind(item_ut=rownames(doc_topic),as.data.frame(doc_topic))
  taggingtest_doc_sc <- unique(papers_tags_df[,c("item_ut","subject_category")])
  taggingtest_data <- merge(taggingtest_doc_topic, taggingtest_doc_sc)
  # plot report
  doc.tagging.test(taggingtest_data = taggingtest_data,filename = filename,path = paste(path,"taggingtest",sep = "/"),LeaveOneOut = LeaveOneOut)
}
#####
# required functions
#####
source("code/functions.R")