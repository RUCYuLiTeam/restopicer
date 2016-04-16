##### analysis for current mission #####
showPreferencePath <- function(username,use.color=T){
  currentMission <- getCurrentMissionInfo(username = username)
  mission_id <- currentMission$mission_id
  mission_round <- currentMission$mission_round - 1
  # get connect
  conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile")
  #dbListTables(conn)
  # get All recmmended papers
  res <- dbSendQuery(conn, paste("SELECT * FROM preference_paper WHERE mission_id = ",mission_id,sep = ""))
  recommendedPapers <- dbFetch(res,n = -1)
  dbClearResult(res)
  dbDisconnect(conn)
  rated_papers <- recommendedPapers[recommendedPapers$rating != -1,]
  if(mission_round<=1) return(NULL)
  # preprocess for rated papers
  result_rated <- searchingByItemUT(papers = rated_papers[rated_papers$rating!=-1,"item_ut"])
  corpus_rated <- preprocess.abstract.corpus(result_lst = result_rated)
  # generate topic by LDA
  train_doc <- posterior(object = result_LDA_abstarct_VEM$corpus_topic,newdata = corpus_rated)
  # build elastic model and get coef
  round_coef_matrix <- NULL
  for(round in 1:mission_round){
    enetmodel <- enet(x = I(train_doc$topics[rated_papers$mission_round<=round,]),
                      y = rated_papers$rating[rated_papers$mission_round<=round],
                      lambda=0.5,normalize = F,intercept = T)
    coef <- predict.enet(enetmodel, s=0.5, type="coef", mode="fraction")
    round_coef_matrix <- rbind(round_coef_matrix,coef$coefficients)
  }
  # ploting
  low <- min(round_coef_matrix) - 0.01 * abs(min(round_coef_matrix))
  up <- max(round_coef_matrix) + 0.01 * abs(max(round_coef_matrix))
  ylimit <- c(low, up)
  plot(0:mission_round, 0:mission_round, 
       xlab = "Round", ylab = "Standardized Preference Coefficients", 
       ylim = ylimit, type = "n")
  for (i in 1:ncol(round_coef_matrix)) {
    if (use.color) {
      lines(0:mission_round, c(0,round_coef_matrix[, i]), col = i, lty = 1)
    }
    else {
      lines(0:mission_round,  c(0,round_coef_matrix[, i]), lty = 1)
    }
  }
  if (!is.null(colnames(round_coef_matrix))) {
    axis(4, at = round_coef_matrix[nrow(round_coef_matrix), ], 
       labels = colnames(round_coef_matrix), cex = 0.8, adj = 0)
  }
  abline(h = 0, lty = 3)
}
# c("fraction", "penalty", "L1norm", "step") for xvar
showLASSOPath <- function(username,showround=0,xvar = "fraction",use.color=T){
  currentMission <- getCurrentMissionInfo(username = username)
  mission_id <- currentMission$mission_id
  mission_round <- currentMission$mission_round - 1
  # default
  if(showround<1) showround <- mission_round
  if(showround<1) return(NULL)
  # get connect
  conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile")
  #dbListTables(conn)
  # get All recmmended papers
  res <- dbSendQuery(conn, paste("SELECT * FROM preference_paper WHERE mission_id = ",mission_id,sep = ""))
  recommendedPapers <- dbFetch(res,n = -1)
  dbClearResult(res)
  dbDisconnect(conn)
  rated_papers <- recommendedPapers[recommendedPapers$rating != -1,]
  # preprocess for rated papers
  result_rated <- searchingByItemUT(papers = rated_papers[rated_papers$rating!=-1,"item_ut"])
  corpus_rated <- preprocess.abstract.corpus(result_lst = result_rated)
  # generate topic by LDA
  train_doc <- posterior(object = result_LDA_abstarct_VEM$corpus_topic,newdata = corpus_rated)
  # build elastic model and get coef
  enetmodel <- enet(x = I(train_doc$topics[rated_papers$mission_round<=showround,]),
                    y = rated_papers$rating[rated_papers$mission_round<=showround],
                    lambda=0.5,normalize = F,intercept = T)
  plot.enet(enetmodel,xvar = xvar,use.color = use.color)
}
showERRORPath <- function(username){
  currentMission <- getCurrentMissionInfo(username = username)
  mission_id <- currentMission$mission_id
  mission_round <- currentMission$mission_round - 1
  # get connect
  conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile")
  #dbListTables(conn)
  # get All recmmended papers
  res <- dbSendQuery(conn, paste("SELECT * FROM preference_paper WHERE mission_id = ",mission_id,sep = ""))
  recommendedPapers <- dbFetch(res,n = -1)
  dbClearResult(res)
  dbDisconnect(conn)
  rated_papers <- recommendedPapers[recommendedPapers$rating != -1,]
  if(mission_round<=1) return(NULL)
  # preprocess for rated papers
  result_rated <- searchingByItemUT(papers = rated_papers[rated_papers$rating!=-1,"item_ut"])
  corpus_rated <- preprocess.abstract.corpus(result_lst = result_rated)
  # generate topic by LDA
  train_doc <- posterior(object = result_LDA_abstarct_VEM$corpus_topic,newdata = corpus_rated)
  # build elastic model and get mean square error
  fit_error <- c()
  for(round in 1:(mission_round-1)){
#     enetmodel <- enet(x = I(train_doc$topics[rated_papers$mission_round<=round,]),
#                       y = rated_papers$rating[rated_papers$mission_round<=round],
#                       lambda=0.5,normalize = F,intercept = T)
#     fits <- predict.enet(object = enetmodel,
#                          newx = train_doc$topics[rated_papers$mission_round>=round+1,],
#                          s = 0.5,type = "fit",mode = "fraction")
    #fit_error <- c(fit_error,mean((fits$fit - rated_papers$rating[rated_papers$mission_round>=round+1])^2))
    #fit_error <- c(fit_error,mean((fits$fit - max(5,fits$fit))^2))
    enetmodel <- enet(x = I(train_doc$topics[rated_papers$mission_round<=round,]),
                      y = rated_papers$rating[rated_papers$mission_round<=round],
                      lambda=0.5,normalize = F,intercept = T)
    fits <- predict.enet(object = enetmodel,
                         newx = train_doc$topics,
                         s = 0.5,type = "fit",mode = "fraction")
    fit_error <- c(fit_error,mean((fits$fit - rated_papers$rating)^2))
  }
  plot(x = 1:(mission_round-1),y = fit_error,type = "o",
       xlab = "Round",ylab = "Mean Square Error")
}
showPreferenceWordCloud <- function(username,showround=0){
  currentMission <- getCurrentMissionInfo(username = username)
  mission_id <- currentMission$mission_id
  mission_round <- currentMission$mission_round
  path <- "E:/phpStudy/WWW/restopicer/sites/all/modules/custom/restopicer/images"
  filename<-paste(path,username,sep="/")
  dir.create(filename)
  if(currentMission$mission_round==1){
    conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile")
    #dbListTables(conn)
    # get All recmmended papers
    res <- dbSendQuery(conn, paste("SELECT * FROM preference_paper WHERE mission_id = ",mission_id,sep = ""))
    recommendedPapers <- dbFetch(res,n = -1)
    dbClearResult(res)
    # get All preference keywords
    res <- dbSendQuery(conn, paste("SELECT * FROM preference_keyword WHERE mission_id = ",mission_id))
    preferenceKeywords <- dbFetch(res,n = -1)
    dbClearResult(res)
#    result_relevent <- searchingByKeywords(keywords = preferenceKeywords$keyword,item_ut_already_list=recommendedPapers$item_ut,relevent_N = relevent_N,preferenceKeywords=preferenceKeywords)
    # retrieve by recommender (composite_N)
#     doRecommend <- getRecommender(recommendername = recommendername)
#     result <- doRecommend(result_relevent=result_relevent,rated_papers=recommendedPapers,composite_N=composite_N,mission_round=mission_round)
#     result_item<- sapply(result,function(v) return (v[1]))
#     result_weightedHybrid<- sapply(result,function(v) return (v[12]))
#     result_weightedHybrid<- unlist(result_weightedHybrid)
#     dm<- sapply(result_item,function(v) return (v[1]))
#     dm<- unlist(dm)
    rated_papers <- recommendedPapers
    # preprocess for rated papers
    result_rated <- searchingByItemUT(papers = rated_papers$item_ut)
    #rated_papers <- recommendedPapers
    # preprocess for rated papers
    #result_rated <- searchingByItemUT(papers = dm)
    corpus_rated <- preprocess.abstract.corpus(result_lst = result_rated)
    # generate topic by LDA
    train_doc <- posterior(object = result_LDA_abstarct_VEM$corpus_topic,newdata = corpus_rated)
    # build elastic model and get coef
    enetmodel <- enet(x = I(train_doc$topics[1:5,]),
                      y = rated_papers$rec_score,
                      lambda=0.5,normalize = F,intercept = T)
    coef <- predict.enet(enetmodel, s=0.5, type="coef", mode="fraction")
    tmp <- coef$coefficients %*% train_doc$terms - min(coef$coefficients %*% train_doc$terms)
    tmp <- as.numeric(tmp)
    names(tmp) <- colnames(train_doc$terms)
    tmp<-tmp[order(tmp,decreasing = T)][1:100]
    i <- seq(from = 2, to = 0.01,by=-0.02)
    tmp[1:100] <- seq(from = 100, to = 1,by=-1)
    tmp <- tmp*i
    tmp[1] <- 300
    
    #tmp <- (tmp + mean(tmp))*10+1
    #tmp<- tmp^6
    #tmp<- tmp+min(tmp)
    #summary(tmp)
    # word cloud
    png(file.path(path, paste(username , paste(mission_round,".jpg",sep=""),sep="/")))
    par(fig = c(0,1,0,1),mar = c(0,0,0,0))
    #pal <- brewer.pal(9,"Blues")[4:9]
    #color_cluster <- pal[ceiling(6*(log(tmp)/max(log(tmp))))]
    wordcloud(words=names(tmp),freq=tmp,scale = c(3.2, 0.5),max.words = 100,
              random.order=F,random.color=F,colors=rainbow(100),ordered.colors=F,
              use.r.layout=F)
    dev.off()
    par(fig = c(0,1,0,1),mar = c(0,0,0,0))
    #pal <- brewer.pal(9,"Blues")[4:9]
    #color_cluster <- pal[ceiling(6*(log(tmp)/max(log(tmp))))]
    wordcloud(words=names(tmp),freq=tmp,scale = c(3.2, 0.5),max.words = 100,
              random.order=F,random.color=F,colors=rainbow(100),ordered.colors=F,
              use.r.layout=F)
  }
  else{
    mission_round <- currentMission$mission_round - 1
    # default
    if(showround<1) showround <- mission_round
    if(showround<1) return(NULL)
    # get connect
    conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile")
    #dbListTables(conn)
    # get All recmmended papers
    res <- dbSendQuery(conn, paste("SELECT * FROM preference_paper WHERE mission_id = ",mission_id,sep = ""))
    recommendedPapers <- dbFetch(res,n = -1)
    dbClearResult(res)
    dbDisconnect(conn)
    rated_papers <- recommendedPapers[recommendedPapers$rating != -1,]
    # preprocess for rated papers
    result_rated <- searchingByItemUT(papers = rated_papers[rated_papers$rating!=-1,"item_ut"])
    corpus_rated <- preprocess.abstract.corpus(result_lst = result_rated)
    # generate topic by LDA
    train_doc <- posterior(object = result_LDA_abstarct_VEM$corpus_topic,newdata = corpus_rated)
    # build elastic model and get coef
    rated_bool <- (rated_papers$rating!=-1)
    if(length(unique(rated_papers$rating[rated_bool]))==1){
      normrating <- rnorm(length(rated_bool), mean = 0, sd = 0.5)
      rated_papers$rating[rated_bool] <- rated_papers$rating[rated_bool] + normrating
      #rated_papers$rating[rated_bool] <- rated_papers$rating[rated_bool] - min(rated_papers$rating[rated_bool])
    }
    enetmodel <- enet(x = I(train_doc$topics[rated_papers$mission_round<=showround,]),
                      y = rated_papers$rating[rated_papers$mission_round<=showround],
                      lambda=0.5,normalize = F,intercept = T)
    coef <- predict.enet(enetmodel, s=0.5, type="coef", mode="fraction")
    tmp <- coef$coefficients %*% train_doc$terms - min(coef$coefficients %*% train_doc$terms)
    tmp <- as.numeric(tmp)
    tmp<- tmp/max(tmp)
    names(tmp) <- colnames(train_doc$terms)
    tmp<-tmp[order(tmp,decreasing = T)][1:100]
    i <- seq(from = 2, to = 0.01,by=-0.02)
    tmp[1:100] <- seq(from = 100, to = 1,by=-1)
    tmp <- tmp*i
    tmp[1] <- 300
    #tmp <- (tmp + mean(tmp))*10+1
    #tmp<- tmp^6
    #tmp<- tmp/max(tmp)
    #tmp<- tmp+min(tmp)
    #summary(tmp)
    # word cloud
    roundname<- mission_round+1
    png(file.path(path, paste(username , paste(roundname ,".jpg",sep=""),sep="/")))
    par(fig = c(0,1,0,1),mar = c(0,0,0,0))
    #pal <- brewer.pal(9,"Blues")[4:9]
    #color_cluster <- pal[ceiling(6*(log(tmp)/max(log(tmp))))]
    wordcloud(words=names(tmp),freq=tmp,scale = c(3.2, 0.5),max.words = 100,
              random.order=F,random.color=F,colors=rainbow(100),ordered.colors=F,
              use.r.layout=F)
    dev.off()
    par(fig = c(0,1,0,1),mar = c(0,0,0,0))
    #pal <- brewer.pal(9,"Blues")[4:9]
    #color_cluster <- pal[ceiling(6*(log(tmp)/max(log(tmp))))]
    wordcloud(words=names(tmp),freq=tmp,scale = c(3.2, 0.5),max.words = 100,
              random.order=F,random.color=F,colors=rainbow(100),ordered.colors=F,
              use.r.layout=F)
    #par(fig = c(0,1,0,0.1), mar = c(0, 1, 0, 1), new=TRUE)
    #display.brewer.pal(9, "Blues")
  }
    mission_round
}
showLastPreferenceWordCloud <- function(username,showround=0){
  currentMission <- getCurrentMissionInfo(username = username)
  mission_id <- currentMission$mission_id
  mission_round_for_judge <- currentMission$mission_round
  if(currentMission$mission_round==2){
    conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile")
    #dbListTables(conn)
    # get All recmmended papers
    res <- dbSendQuery(conn, paste("SELECT * FROM preference_paper WHERE mission_id = ",mission_id,sep = ""))
    recommendedPapers <- dbFetch(res,n = -1)
    dbClearResult(res)
    dbDisconnect(conn)
    rated_papers <- recommendedPapers[recommendedPapers$rating != -1,]
    # preprocess for rated papers
    result_rated <- searchingByItemUT(papers = rated_papers[rated_papers$rating!=-1,"item_ut"])
    corpus_rated <- preprocess.abstract.corpus(result_lst = result_rated)
    # generate topic by LDA
    train_doc <- posterior(object = result_LDA_abstarct_VEM$corpus_topic,newdata = corpus_rated)
    # build elastic model and get coef
    enetmodel <- enet(x = I(train_doc$topics[1:5,]),
                      y = rated_papers$rec_score[1:5],
                      lambda=0.5,normalize = F,intercept = T)
    coef <- predict.enet(enetmodel, s=0.5, type="coef", mode="fraction")
    tmp <- coef$coefficients %*% train_doc$terms - min(coef$coefficients %*% train_doc$terms)
    tmp <- as.numeric(tmp)
    tmp<- tmp/max(tmp)
    names(tmp) <- colnames(train_doc$terms)
    tmp<-tmp[order(tmp,decreasing = T)][1:100]
    i <- seq(from = 2, to = 0.01,by=-0.02)
    tmp[1:100] <- seq(from = 100, to = 1,by=-1)
    tmp <- tmp*i
    tmp[1] <- 300
    #tmp <- (tmp + mean(tmp))*10+1
    #tmp<- tmp^6
    #tmp<- tmp/max(tmp)
    #tmp<- tmp+min(tmp)
    #summary(tmp)
    # word cloud
    par(fig = c(0,1,0,1),mar = c(0,0,0,0))
    #pal <- brewer.pal(9,"Blues")[4:9]
    #color_cluster <- pal[ceiling(6*(log(tmp)/max(log(tmp))))]
    wordcloud(words=names(tmp),freq=tmp,scale = c(3.2, 0.5),max.words = 100,
              random.order=F,random.color=F,colors=rainbow(100),ordered.colors=F,
              use.r.layout=F)
    #par(fig = c(0,1,0,0.1), mar = c(0, 1, 0, 1), new=TRUE)
    #display.brewer.pal(9, "Blues")
  }else{
    mission_round <- currentMission$mission_round - 1
    # default
    if(showround<1) showround <- mission_round-1
    if(showround<1) return(mission_round_for_judge)
    # get connect
    conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile")
    #dbListTables(conn)
    # get All recmmended papers
    res <- dbSendQuery(conn, paste("SELECT * FROM preference_paper WHERE mission_id = ",mission_id," And mission_round =",mission_round,sep = ""))
    recommendedPapers <- dbFetch(res,n = -1)
    dbClearResult(res)
    dbDisconnect(conn)
    rated_papers <- recommendedPapers[recommendedPapers$rating != -1,]
    # preprocess for rated papers
    result_rated <- searchingByItemUT(papers = rated_papers[rated_papers$rating!=-1,"item_ut"])
    corpus_rated <- preprocess.abstract.corpus(result_lst = result_rated)
    # generate topic by LDA
    train_doc <- posterior(object = result_LDA_abstarct_VEM$corpus_topic,newdata = corpus_rated)
    # build elastic model and get coef
    rated_bool <- (rated_papers$rating!=-1)
    if(length(unique(rated_papers$rating[rated_bool]))==1){
      normrating <- rnorm(length(rated_bool), mean = 0, sd = 0.5)
      rated_papers$rating[rated_bool] <- rated_papers$rating[rated_bool] + normrating
      #rated_papers$rating[rated_bool] <- rated_papers$rating[rated_bool] - min(rated_papers$rating[rated_bool])
    }
    enetmodel <- enet(x = I(train_doc$topics),
                      y = rated_papers$rating,
                      lambda=0.5,normalize = F,intercept = T)
    coef <- predict.enet(enetmodel, s=0.5, type="coef", mode="fraction")
    tmp <- coef$coefficients %*% train_doc$terms - min(coef$coefficients %*% train_doc$terms)
    tmp <- as.numeric(tmp)
    names(tmp) <- colnames(train_doc$terms)
    tmp<-tmp[order(tmp,decreasing = T)][1:100]
    i <- seq(from = 2, to = 0.01,by=-0.02)
    tmp[1:100] <- seq(from = 100, to = 1,by=-1)
    tmp <- tmp*i
    tmp[1] <- 300
    #tmp <- (tmp + mean(tmp))*10+1
    #tmp<- tmp^6
    #tmp<- tmp/max(tmp)
    #tmp<- tmp+min(tmp)
    #summary(tmp)
    # word cloud
    par(fig = c(0,1,0,1),mar = c(0,0,0,0))
    #pal <- brewer.pal(9,"Blues")[4:9]
    #color_cluster <- pal[ceiling(6*(log(tmp)/max(log(tmp))))]
    wordcloud(words=names(tmp),freq=tmp,scale = c(3.2, 0.5),max.words = 100,
              random.order=F,random.color=F,colors=rainbow(100),ordered.colors=F,
              use.r.layout=F)
   # par(fig = c(0,1,0,0.1), mar = c(0, 1, 0, 1), new=TRUE)
   #display.brewer.pal(9, "Blues")
  }

}
showEachRoundPreferenceWordCloud <- function(username,showround=0){
  currentMission <- getCurrentMissionInfo(username = username)
  maxratedmission_round<-getMaxRatedMissionRound(username = username)
  mission_id <- currentMission$mission_id
  mission_round <- currentMission$mission_round
    # default
    if(showround<1) showround <- maxratedmission_round
    if(showround<1) return(NULL)
  #split.screen(c(1,maxratedmission_round))
  #layout=c(1,maxratedmission_round)
  mymatrix<-matrix(0,nrow =maxratedmission_round,ncol = maxratedmission_round )
  for(i in 1:maxratedmission_round){
    for(j in 1:maxratedmission_round){
      mymatrix[i,j]=i*j
    }
    
  }
  if(maxratedmission_round==1){
    par(mfrow = c(1,1))
  }else if(maxratedmission_round==2){
    par(mfrow = c(1,2))
  }else{
    minmatrix<- min(mymatrix[mymatrix > maxratedmission_round])
    location<- which(mymatrix==minmatrix,arr.ind = T)
    minus_location<- abs(location[,1]-location[,2])
    row_loc<- which(minus_location==min(minus_location), arr.ind = 1)
    row<- location[row_loc[1],1]
    col<- location[row_loc[1],2]
    par(mfrow = c(row,col))
  }
  for(i in 1:maxratedmission_round){
    # get connect
    conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile")
    #dbListTables(conn)
    # get All recmmended papers
    res <- dbSendQuery(conn, paste("SELECT * FROM preference_paper WHERE mission_id = ",mission_id," AND mission_round <= ",i,sep = ""))
    recommendedPapers <- dbFetch(res,n = -1)
    dbClearResult(res)
    dbDisconnect(conn)
    rated_papers <- recommendedPapers[recommendedPapers$rating != -1,]
    # preprocess for rated papers
    result_rated <- searchingByItemUT(papers = rated_papers[rated_papers$rating!=-1,"item_ut"])
    corpus_rated <- preprocess.abstract.corpus(result_lst = result_rated)
    # generate topic by LDA
    train_doc <- posterior(object = result_LDA_abstarct_VEM$corpus_topic,newdata = corpus_rated)
    # build elastic model and get coef
    rated_bool <- (rated_papers$rating!=-1)
    if(length(unique(rated_papers$rating[rated_bool]))==1){
      normrating <- rnorm(length(rated_bool), mean = 0, sd = 0.5)
      rated_papers$rating[rated_bool] <- rated_papers$rating[rated_bool] + normrating
      #rated_papers$rating[rated_bool] <- rated_papers$rating[rated_bool] - min(rated_papers$rating[rated_bool])
    }
    enetmodel <- enet(x = I(train_doc$topics[rated_papers$mission_round<=showround,]),
                      y = rated_papers$rating[rated_papers$mission_round<=showround],
                      lambda=0.5,normalize = F,intercept = T)
    coef <- predict.enet(enetmodel, s=0.5, type="coef", mode="fraction")
    tmp <- coef$coefficients %*% train_doc$terms - min(coef$coefficients %*% train_doc$terms)
    tmp <- as.numeric(tmp)
    names(tmp) <- colnames(train_doc$terms)
    tmp <- (tmp + mean(tmp))*10+1
    tmp<- tmp^6
    #tmp<- tmp+min(tmp)
    #summary(tmp)
    # word cloud
    #par(fig = c(0,1,0.1,1),mar = c(0,0,0,0))
    par(pin = c(60,80),mar = c(0,0,0,0))
    pal <- brewer.pal(9,"Blues")[4:9]
    color_cluster <- pal[ceiling(6*(log(tmp)/max(log(tmp))))]
    #screen(i)
    wordcloud(words=names(tmp),freq=tmp,scale = c(2, 0),min.freq=1,max.words = 40,
              random.order=F,random.color=F,rot.per=0,colors=rainbow(100),ordered.colors=F,
              use.r.layout=F,fixed.asp=F)
  }
}
showRatingPath <- function(username){
  currentMission <- getCurrentMissionInfo(username = username)
  mission_id <- currentMission$mission_id
  mission_round <- currentMission$mission_round - 1
  # get connect
  conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile")
  #dbListTables(conn)
  # get All recmmended papers
  res <- dbSendQuery(conn, paste("SELECT * FROM preference_paper WHERE mission_id = ",mission_id,sep = ""))
  recommendedPapers <- dbFetch(res,n = -1)
  dbClearResult(res)
  dbDisconnect(conn)
  rated_papers <- recommendedPapers[recommendedPapers$rating != -1,]
  if(mission_round<=1) return(NULL)
  # get average ratings
  df <- rated_papers %>% group_by(mission_round) %>% summarize(mrating=mean(scale(rating,center = F)))
  plot(x = df$mission_round,y = df$mrating,type = "o",
       xlab = "Round",ylab = "Mean Ratings")
}