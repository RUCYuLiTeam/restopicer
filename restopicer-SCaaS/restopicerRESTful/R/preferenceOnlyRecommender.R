preferenceOnlyRecommend <- function(result_relevent,rated_papers,
                                   topics_filepath,
                                   mission_round,
                                   composite_N,dropped_topic,...){
  # for not new mission to train enet model
  if(mission_round!=1 && !is.null(rated_papers) && nrow(rated_papers)>=2){
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
  rated_id <- unlist(lapply(result_relevent, function(x){
    which(pretrain_doc$item_ut==x$item_ut$item_ut)
  }))
  predict_doc <- list(topics=pretrain_doc$topics[rated_id,],terms=pretrain_doc$terms)
  #dropped topics for enet
  if(length(dropped_topic$topic_drop)!=0)  predict_doc$topics <- predict_doc$topics[,-dropped_topic$topic_drop]
  # get evaluator
  # for preference (rating exploitation)
  doRE <- getPreferenceEvaluator(name = "elasticNetPreferenceEval")
  doLVE <- getExploreValueEvaluator(name = "activeExploreEval")
  # for exploration
  # ready to loop
  relevent_N <- length(result_relevent)
  df_result_relevent <- data.frame(item_ut=character(relevent_N),
                                   exploitation_relevent=numeric(relevent_N),exploitation_rating=numeric(relevent_N),
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
  }
  # cal preference of rating
  if(exists(x = "enetmodel"))  df_result_relevent$exploitation_rating <- doRE(enetmodel = enetmodel,predict_new_docs = predict_doc$topics)
  # scaling
  #df_result_relevent$exploitation_relevent <- scale(df_result_relevent$exploitation_relevent,center = F,scale = T)
  df_result_relevent$exploitation_relevent <- 10*score_scaling(df_result_relevent$exploitation_relevent,min_scale=0.2)
  #df_result_relevent$exploitation_rating <- scale(df_result_relevent$exploitation_rating,center = F,scale = T)
  df_result_relevent$exploitation_rating <- 10*score_scaling(df_result_relevent$exploitation_rating,x_min=1,x_max=5,min_scale=0.1)
  # cal control weight
  weight_lst <- list(exploitation_relevent_w=0.5,
                     exploitation_rating_w=0.5)
  # cal the hybrid weight
  df_result_relevent$weightedHybrid <- 
    df_result_relevent$exploitation_relevent * weight_lst$exploitation_relevent_w +
    df_result_relevent$exploitation_rating * weight_lst$exploitation_rating_w
  
  df_result_relevent$weightedHybrid <- round(df_result_relevent$weightedHybrid,2)
  for(i in 1:length(result_relevent)){
    relevent_title <- result_relevent[[i]]$item_ut
    # get weightHybrid
    result_relevent[[i]]$weightedHybrid <- df_result_relevent[which(df_result_relevent$item_ut==relevent_title),"weightedHybrid"]
    result_relevent[[i]]$relevent <- df_result_relevent[which(df_result_relevent$item_ut==relevent_title),"exploitation_relevent"]
    result_relevent[[i]]$pred_rating <- df_result_relevent[which(df_result_relevent$item_ut==relevent_title),"exploitation_rating"]
  }
  
  result_relevent[order(df_result_relevent$weightedHybrid,decreasing = T)[1:min(length(result_relevent),composite_N)]]
}
# scaling method for [0,1] or [min_scale,1]
score_scaling <- function(x,x_min=min(x),x_max=max(x),min_scale=0.1){
  scale_multiply <- min_scale/(1 - min_scale)
  delta <- (x_max - x_min) * scale_multiply
  (x - x_min + delta)/(x_max - x_min + delta)
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
