exploreHybridRecommend <- function(result_relevent,rated_papers,
                                   topics_filepath,
                                   mission_round,
                                   composite_N,...){
  # for not new mission to train enet model
  if(mission_round!=1 && !is.null(rated_papers) && nrow(rated_papers)>=2){
    # preprocess for relevent and rated papers
    result_rated <- searchingByItemUT(papers = rated_papers[rated_papers$rating!=-1,"item_ut"])
    corpus_rated <- preprocess.abstract.corpus(result_lst = result_rated)
    # generate topic by LDA
    train_doc <- posterior(object = result_LDA_abstarct_VEM$corpus_topic,newdata = corpus_rated)
    # model bug fix needed (new feature selection method, should not use LASSO)
    # all the rating are equal
    rated_bool <- (rated_papers$rating!=-1)
    if(length(unique(rated_papers$rating[rated_bool]))==1){
      normrating <- rnorm(length(rated_bool), mean = 0, sd = 0.5)
      rated_papers$rating[rated_bool] <- rated_papers$rating[rated_bool] + normrating
      #rated_papers$rating[rated_bool] <- rated_papers$rating[rated_bool] - min(rated_papers$rating[rated_bool])
    }
    # build elastic model
    enetmodel <- enet(x = I(train_doc$topics),y = rated_papers$rating,lambda=0.5,normalize = F,intercept = T)
    #plot(enetmodel)
  }
  # generate topic by LDA and preprocess for relevent and rated papers
  # http://stats.stackexchange.com/questions/9315/topic-prediction-using-latent-dirichlet-allocation
  corpus_relevent <- preprocess.abstract.corpus(result_lst = result_relevent)
  predict_doc <- posterior(object = result_LDA_abstarct_VEM$corpus_topic,newdata = corpus_relevent)
  # get evaluator
  # for preference (rating exploitation)
  doRE <- getRatingEvaluator(name = "elasticNetPreferenceEval")
  # for exploration
  doLVE <- getLearnValueEvaluator(name = "activeExploreEval")
  doQE <- getQualityEvaluator(name = "SCIImpactFactorEval")
  doSVE <- getSummaryValueEvaluator(name = "topicEntropySummaryEval")
  doFE <- getFreshEvaluator(name = "yearDiffReciprocalFreshEval")
  # ready to loop
  df_result_relevent <- data.frame()
  for(i in 1:length(result_relevent)){
    relevent_lst <- result_relevent[[i]]
    # cal learn_ability
    learn_ability <- 1
    if(exists(x = "enetmodel")){
      learn_ability <- doLVE(enetmodel = enetmodel,
                                   new_doc_i = i,test_docs = predict_doc$topics,
                                   train_docs = train_doc$topics,train_rating = rated_papers$rating)
    }
    # rbind (should not sorted)
    df_result_relevent <- rbind(df_result_relevent,
                                data.frame(item_ut=relevent_lst$item_ut$item_ut,
                                           exploitation_relevent = relevent_lst$score,
                                           exploitation_rating = 1,
                                           exploration_learn = learn_ability,
                                           exploration_quality = doQE(magazine = relevent_lst$magazine$full_source_title),
                                           exploration_summary = doSVE(z = predict_doc$topics[i,]),
                                           exploration_fresh = doFE(publication_year = relevent_lst$publication_year$publication_year),
                                           row.names = F,stringsAsFactors = F))
  }
  # cal preference of rating
  if(exists(x = "enetmodel"))  df_result_relevent$exploitation_rating <- doRE(enetmodel = enetmodel,predict_new_docs = predict_doc$topics)
  # scaling
  df_result_relevent$exploitation_relevent <- scale(df_result_relevent$exploitation_relevent,center = F,scale = T)
  df_result_relevent$exploitation_relevent <- df_result_relevent$exploitation_relevent/max(df_result_relevent$exploitation_relevent)
  df_result_relevent$exploitation_rating <- scale(df_result_relevent$exploitation_rating,center = F,scale = T)
  df_result_relevent$exploitation_rating <- df_result_relevent$exploitation_rating/abs(max(df_result_relevent$exploitation_rating))
  df_result_relevent$exploration_learn <- scale(df_result_relevent$exploration_learn,center = F,scale = T)
  df_result_relevent$exploration_learn <- df_result_relevent$exploration_learn/max(df_result_relevent$exploration_learn)
  df_result_relevent$exploration_quality <- scale(df_result_relevent$exploration_quality,center = F,scale = T)
  df_result_relevent$exploration_quality <- df_result_relevent$exploration_quality/max(df_result_relevent$exploration_quality)
  df_result_relevent$exploration_summary <- scale(df_result_relevent$exploration_summary,center = F,scale = T)
  df_result_relevent$exploration_summary <- df_result_relevent$exploration_summary/max( df_result_relevent$exploration_summary)
  df_result_relevent$exploration_fresh <- df_result_relevent$exploration_fresh/max(df_result_relevent$exploration_fresh)
  # cal control weight
  doRecommenderControl <- getRecommendController(controllername = "hybridWeightControl")
  weight_lst <- doRecommenderControl(mission_round)
  # cal the hybrid weight
  df_result_relevent$weightedHybrid <- 
    df_result_relevent$exploitation_relevent * weight_lst$exploitation_relevent_w +
    df_result_relevent$exploitation_rating * weight_lst$exploitation_rating_w +
    df_result_relevent$exploration_learn * weight_lst$exploration_learn_w +
    df_result_relevent$exploration_quality * weight_lst$exploration_quality_w +
    df_result_relevent$exploration_summary * weight_lst$exploration_summary_w +
    df_result_relevent$exploration_fresh * weight_lst$exploration_fresh_w
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
