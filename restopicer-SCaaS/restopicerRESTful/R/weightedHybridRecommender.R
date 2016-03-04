weightedHybridRecommend <- function(result_relevent,rated_papers,
                                    topics_filepath,
                                    mission_round,
                                    #preference_w,explore_w,quality_w,summary_w,fresh_w,
                                    composite_N,...){
  # for not new mission
  if(mission_round!=1 && !is.null(rated_papers) && nrow(rated_papers)>=2){
    # preprocess for relevent and rated papers
    result_rated <- searchingByItemUT(papers = rated_papers[rated_papers$rating!=-1,"item_ut"])
    corpus_rated <- preprocess.abstract.corpus(result_lst = result_rated)
    # generate topic by LDA
    train_doc <- posterior(object = result_LDA_abstarct_VEM$corpus_topic,newdata = corpus_rated)
    # build elastic model
    enetmodel <- enet(x = I(train_doc$topics),y = rated_papers$rating,lambda=0.5,normalize = F,intercept = T)
    #plot(enetmodel)
  }
  # generate topic by LDA and preprocess for relevent and rated papers
  corpus_relevent <- preprocess.abstract.corpus(result_lst = result_relevent)
  predict_doc <- posterior(object = result_LDA_abstarct_VEM$corpus_topic,newdata = corpus_relevent)
  # get evaluator
  # for preference (exploitation)
  doPE <- getPreferenceEvaluator(name = "elasticNetPreferenceEval")
  # for exploration
  doEVE <- getExploreValueEvaluator(name = "activeExploreEval")
  doQE <- getQualityEvaluator(name = "SCIImpactFactorEval")
  doSVE <- getSummaryValueEvaluator(name = "topicEntropySummaryEval")
  doFE <- getFreshEvaluator(name = "yearDiffReciprocalFreshEval")
  # ready to loop
  df_result_relevent <- data.frame()
  for(i in 1:length(result_relevent)){
    relevent_lst <- result_relevent[[i]]
    # cal exploration_ability
    if(exists(x = "enetmodel")){
      exploration_ability <- doEVE(enetmodel = enetmodel,
                                   new_doc_i = i,test_docs = predict_doc$topics,
                                   train_docs = train_doc$topics,train_rating = rated_papers$rating)
    }
    # rbind (should not sorted)
    df_result_relevent <- rbind(df_result_relevent,
      data.frame(item_ut=relevent_lst$item_ut$item_ut,
               preference = 1,
               exploration_ability = exploration_ability,
               quality_index = doQE(magazine = relevent_lst$magazine$full_source_title),
               summary_index = doSVE(z = predict_doc$topics[i,]),
               fresh_index = doFE(publication_year = relevent_lst$publication_year$publication_year),
               row.names = F,stringsAsFactors = F))
  }
  # cal preference
  if(exists(x = "enetmodel"))  df_result_relevent$preference <- doPE(enetmodel = enetmodel,predict_new_docs = predict_doc$topics)
  # scaling
  df_result_relevent$preference <- scale(df_result_relevent$preference,center = F,scale = T)
  df_result_relevent$exploration_ability <- scale(df_result_relevent$exploration_ability,center = F,scale = T)
  df_result_relevent$quality_index <- scale(df_result_relevent$quality_index,center = F,scale = T)
  df_result_relevent$summary_index <- scale(df_result_relevent$summary_index,center = F,scale = T)
  df_result_relevent$fresh_index <- scale(df_result_relevent$fresh_index,center = F,scale = T)
  # cal control weight
  doRecommenderControl <- getRecommendController(controllername = "simpleWeightControl")
  weight_lst <- doRecommenderControl(mission_round)
  # cal the hybrid weight
  df_result_relevent$weightedHybrid <- 
    df_result_relevent$preference * weight_lst$preference_w +
    df_result_relevent$exploration_ability * weight_lst$explore_w +
    df_result_relevent$quality_index * weight_lst$quality_w +
    df_result_relevent$summary_index * weight_lst$summary_w +
    df_result_relevent$fresh_index * weight_lst$fresh_w
  result_relevent[order(df_result_relevent$weightedHybrid,decreasing = T)[1:min(length(result_relevent),composite_N)]]
}
# preprocessing for abstract corpus
preprocess.abstract.corpus <- function(result_lst){
  papers_df <- rbind_all(lapply(result_lst, function(x){
    data.frame(item_ut=x$item_ut$item_ut,abstract=paste(x$article_title,x$abstract$abstract,unlist(x$keywords$keywords),x$magazine$full_source_title,sep = " ",collapse = " "))
  }))
  data <- unique(papers_df)
  corpus <- VCorpus(VectorSource(data$abstract))
  # fpattern <- content_transformer(function(x, pattern) gsub(pattern, "", x))
  # complainCorpus <- tm_map(complainCorpus, fpattern, "z*")
  corpus <- tm_map(corpus, removeWords, c("Elsevier B.V. All rights reserved"))
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
