exploreHybridRecommend <- function(result_relevent,rated_papers,
                                   topics_filepath,
                                   mission_round,
                                   composite_N,dropped_topic,controllername = "simpleHybridWeightControl",...){
  # for not new mission to train enet model
  if(mission_round>=2 && !is.null(rated_papers) && nrow(rated_papers)>=2){
    # preprocess for relevent and rated papers
    result_rated <- searchingByItemUT(papers = rated_papers[rated_papers$rating!=-1,"item_ut"])
    #corpus_rated <- preprocess.abstract.corpus(result_lst = result_rated)
    # generate topic by LDA
    #train_doc <- posterior(object = result_LDA_abstarct_VEM$corpus_topic,newdata = corpus_rated)
    rated_id <- unlist(lapply(result_rated, function(x){
      which(pretrain_doc$item_ut==x$item_ut$item_ut)
    }))
    train_doc <- list(topics=pretrain_doc$topics[rated_id,],terms=pretrain_doc$terms)
    #dropped topics for enet
    if(length(dropped_topic$topic_drop)!=0)  train_doc$topics <- train_doc$topics[,-dropped_topic$topic_drop]
    # model bug fix needed (new feature selection method, should not use LASSO)
    # all the rating are equal
    rated_bool <- (rated_papers$rating!=-1)
    if(length(unique(rated_papers$rating[rated_bool]))==1){
      normrating <- rnorm(length(rated_bool), mean = 0, sd = 0.5)
      rated_papers$rating[rated_bool] <- rated_papers$rating[rated_bool] + normrating
      #rated_papers$rating[rated_bool] <- rated_papers$rating[rated_bool] - min(rated_papers$rating[rated_bool])
    }
    # build elastic model
    #enetmodel <- glmnet(x = train_doc$topics,y = rated_papers$rating,family = "gaussian",alpha = 0.5,intercept = F)
    enetmodel <- enet(x = train_doc$topics,y = rated_papers$rating,lambda=0.5,normalize = F,intercept = F)
    #plot(enetmodel)
  }
  # generate topic by LDA and preprocess for relevent and rated papers
  # http://stats.stackexchange.com/questions/9315/topic-prediction-using-latent-dirichlet-allocation
  #corpus_relevent <- preprocess.abstract.corpus(result_lst = result_relevent)
  #predict_doc <- posterior(object = result_LDA_abstarct_VEM$corpus_topic,newdata = corpus_relevent)
  rated_id <- unlist(lapply(result_relevent, function(x){
    which(pretrain_doc$item_ut==x$item_ut$item_ut)
  }))
  predict_doc <- list(topics=pretrain_doc$topics[rated_id,],terms=pretrain_doc$terms)
  #dropped topics for enet
  if(length(dropped_topic$topic_drop)!=0)  predict_doc$topics <- predict_doc$topics[,-dropped_topic$topic_drop]
  # get evaluator
  # for preference (rating exploitation)
  doRE <- getPreferenceEvaluator(name = "elasticNetPreferenceEval")
  # for exploration
  doLVE <- getExploreValueEvaluator(name = "activeExploreEval")
  doQE <- getQualityEvaluator(name = "SCIImpactFactorEval")
  doSVE <- getSummaryValueEvaluator(name = "topicEntropySummaryEval")
  doFE <- getFreshEvaluator(name = "yearDiffReciprocalFreshEval")
  # ready to loop
  relevent_N <- length(result_relevent)
  # using lappy
#   result_relevent_lst <- lapply(1:relevent_N, function(i){
#     relevent_lst <- result_relevent[[i]]
#     learn_ability <- 1
#     if(exists(x = "enetmodel")){
#       learn_ability <- doLVE(enetmodel = enetmodel,
#                              new_doc_i = i,test_docs = predict_doc$topics,
#                              train_docs = train_doc$topics,train_rating = rated_papers$rating)
#     }
#     data.frame(item_ut=relevent_lst$item_ut$item_ut,
#                exploitation_relevent = relevent_lst$score,
#                exploitation_rating = 1,
#                exploitation_quality = doQE(magazine = relevent_lst$magazine$full_source_title),
#                exploration_learn = learn_ability,
#                exploration_summary = doSVE(z = predict_doc$topics[i,]),
#                exploration_fresh = doFE(publication_year = relevent_lst$publication_year$publication_year),
#                row.names = F,stringsAsFactors = F)
#   })
#   df_result_relevent <- as.data.frame(rbindlist(result_relevent_lst))
  # manual loop
  df_result_relevent <- data.frame(item_ut=character(relevent_N),
                                   exploitation_relevent=numeric(relevent_N),exploitation_rating=numeric(relevent_N),exploitation_quality=numeric(relevent_N),
                                   exploration_learn=numeric(relevent_N),exploration_summary=numeric(relevent_N),exploration_fresh=numeric(relevent_N),
                                   stringsAsFactors = F)
  for(i in 1:relevent_N){
    relevent_lst <- result_relevent[[i]]
    # cal learn_ability
    learn_ability <- 0
    if(exists(x = "enetmodel")){
      learn_ability <- doLVE(enetmodel = enetmodel,
                             new_doc_i = i,test_docs = predict_doc$topics,
                             train_docs = train_doc$topics,train_rating = rated_papers$rating)
    }
    df_result_relevent[i,]$item_ut <- relevent_lst$item_ut$item_ut
    df_result_relevent[i,]$exploitation_relevent <- relevent_lst$score
    df_result_relevent[i,]$exploitation_rating <- runif(1,min = 1,max = 5)
    df_result_relevent[i,]$exploitation_quality <- doQE(magazine = relevent_lst$magazine$full_source_title)
    df_result_relevent[i,]$exploration_learn <- learn_ability
    df_result_relevent[i,]$exploration_summary <- doSVE(z = predict_doc$topics[i,])
    df_result_relevent[i,]$exploration_fresh <- doFE(publication_year = relevent_lst$publication_year$publication_year)
  }
  # cal preference of rating
  if(exists(x = "enetmodel"))  df_result_relevent$exploitation_rating <- doRE(enetmodel = enetmodel,predict_new_docs = predict_doc$topics)
  # scaling
  #df_result_relevent$exploitation_relevent <- scale(df_result_relevent$exploitation_relevent,center = F,scale = T)
  df_result_relevent$exploitation_relevent <- 10*score_scaling(df_result_relevent$exploitation_relevent,min_scale=0.2)
  #df_result_relevent$exploitation_rating <- scale(df_result_relevent$exploitation_rating,center = F,scale = T)
  df_result_relevent$exploitation_rating <- 10*score_scaling(df_result_relevent$exploitation_rating,x_min=1,x_max=5,min_scale=0.1)
  #df_result_relevent$exploitation_quality <- scale(df_result_relevent$exploitation_quality,center = F,scale = T)
  df_result_relevent$exploitation_quality <- 10*score_scaling(df_result_relevent$exploitation_quality,x_min=1,x_max=5.311,min_scale=0.3)
  #df_result_relevent$exploration_learn <- scale(df_result_relevent$exploration_learn,center = F,scale = T)
  df_result_relevent$exploration_learn <- zoo::na.fill(10*score_scaling(df_result_relevent$exploration_learn,min_scale=0.1),1)
  #df_result_relevent$exploration_summary <- scale(df_result_relevent$exploration_summary,center = F,scale = T)
  df_result_relevent$exploration_summary <- 10*score_scaling(df_result_relevent$exploration_summary,min_scale=0.1)
  df_result_relevent$exploration_fresh <- 10*score_scaling(df_result_relevent$exploration_fresh,x_min=0,x_max=1,min_scale=0)
  # cal control weight
  doRecommenderControl <- getRecommendController(controllername = controllername)
  weight_lst <- doRecommenderControl(mission_round)
  # cal the hybrid weight
  df_result_relevent$weightedHybrid <- 
    df_result_relevent$exploitation_relevent * weight_lst$exploitation_relevent_w +
    df_result_relevent$exploitation_rating * weight_lst$exploitation_rating_w +
    df_result_relevent$exploration_learn * weight_lst$exploration_learn_w +
    df_result_relevent$exploitation_quality * weight_lst$exploitation_quality_w +
    df_result_relevent$exploration_summary * weight_lst$exploration_summary_w +
    df_result_relevent$exploration_fresh * weight_lst$exploration_fresh_w
  #scale
  df_result_relevent$weightedHybrid_true <- df_result_relevent$weightedHybrid
  #df_result_relevent$weightedHybrid <- scale(df_result_relevent$weightedHybrid,center = F,scale = T)
  #df_result_relevent$weightedHybrid <- 10*df_result_relevent$weightedHybrid/max(df_result_relevent$weightedHybrid)
  result_output <- result_relevent[order(df_result_relevent$weightedHybrid,decreasing = T)[1:min(length(result_relevent),composite_N)]]
  for(i in 1:length(result_output)){
    relevent_title <- result_output[[i]]$item_ut$item_ut
    # get weightHybrid
    result_output[[i]]$mission_round <- mission_round
    result_output[[i]]$weightedHybrid_true <- df_result_relevent[which(df_result_relevent$item_ut==relevent_title),"weightedHybrid_true"]
    #result_output[[i]]$weightedHybrid <- df_result_relevent[which(df_result_relevent$item_ut==relevent_title),"weightedHybrid"]
    result_output[[i]]$relevent <- df_result_relevent[which(df_result_relevent$item_ut==relevent_title),"exploitation_relevent"]
    result_output[[i]]$pred_rating <- df_result_relevent[which(df_result_relevent$item_ut==relevent_title),"exploitation_rating"]
    result_output[[i]]$quality <- df_result_relevent[which(df_result_relevent$item_ut==relevent_title),"exploitation_quality"]
    result_output[[i]]$learn_ability <- df_result_relevent[which(df_result_relevent$item_ut==relevent_title),"exploration_learn"]
    result_output[[i]]$summary_degree <- df_result_relevent[which(df_result_relevent$item_ut==relevent_title),"exploration_summary"]
    result_output[[i]]$fresh <- df_result_relevent[which(df_result_relevent$item_ut==relevent_title),"exploration_fresh"]
    result_output[[i]]$exploration_w <- weight_lst$exploration_w
  }
  #list(result_output=result_output,exploration_w=weight_lst$exploration_w)
  result_output
}
# scaling method for [0,1] or [min_scale,1]
score_scaling <- function(x,x_min=min(x),x_max=max(x),min_scale=0.1){
  scale_multiply <- min_scale/(1 - min_scale)
  delta <- (x_max - x_min) * scale_multiply
  zoo::na.fill((x - x_min + delta)/(x_max - x_min + delta),0.05)
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
  #corpus <- tm_map(corpus, removeWords, c("Elsevier B.V. All rights reserved"))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  #corpus <- tm_map(corpus, removeWords, stopwords("SMART"))
  #corpus <- tm_map(corpus, removeWords, stopwords("en"))
  #corpus <- tm_map(corpus, removeWords, stopwords("service"))
  corpus <- tm_map(corpus, stripWhitespace)
  #strsplit_space_tokenizer <- function(x){
  #  unlist(strsplit(as.character(x), "[[:space:]]+"))
  #}
  # control <- list(weighting = weightTf, tokenize= strsplit_space_tokenizer,
  #                 tolower = TRUE, removePunctuation = TRUE, removeNumbers = TRUE, stopwords = TRUE, stemming = TRUE,
  #                 dictionary = NULL, bounds = list(local = c(1, Inf)), wordLengths = c(3, Inf))
  control <- list(weighting = weightTf, tokenize= words,
                  tolower = TRUE, removePunctuation = TRUE, removeNumbers = TRUE, stopwords = TRUE, stemming = FALSE,
                  dictionary = NULL, bounds = list(local = c(1, Inf)), wordLengths = c(3, Inf))
  corpus_dtm <- DocumentTermMatrix(corpus,control)
  corpus_dtm
}  
#ready to loop
result_relevent_loop<- function(result_relevent){
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
                                         exploitation_quality = doQE(magazine = relevent_lst$magazine$full_source_title),
                                         exploration_summary = doSVE(z = predict_doc$topics[i,]),
                                         exploration_fresh = doFE(publication_year = relevent_lst$publication_year$publication_year),
                                         row.names = F,stringsAsFactors = F))
  }
  return(df_result_relevent)
}
