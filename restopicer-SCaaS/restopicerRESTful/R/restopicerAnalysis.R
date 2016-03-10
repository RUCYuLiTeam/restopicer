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
  for(round in 1:mission_round){
    enetmodel <- enet(x = I(train_doc$topics[rated_papers$mission_round<=round,]),
                      y = rated_papers$rating[rated_papers$mission_round<=round],
                      lambda=0.5,normalize = F,intercept = T)
    fits <- predict.enet(object = enetmodel,
                         newx = train_doc$topics,
                         s = 0.5,type = "fit",mode = "fraction")
    fit_error <- c(fit_error,mean((fits$fit - rated_papers$rating)^2))
  }
  plot(x = 1:mission_round,y = fit_error,type = "o",
       xlab = "Round",ylab = "Fitted Mean Square Error")
}
showPreferenceWordCloud <- function(username,showround=0){
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
  coef <- predict.enet(enetmodel, s=0.5, type="coef", mode="fraction")
  tmp <- coef$coefficients %*% train_doc$terms - min(coef$coefficients %*% train_doc$terms)
  tmp <- as.numeric(tmp)
  names(tmp) <- colnames(train_doc$terms)
  tmp <- (tmp + mean(tmp))*10
  summary(tmp)
  # word cloud
  par(fig = c(0,1,0.1,1),mar = c(0,0,0,0))
  pal <- brewer.pal(9,"Blues")[4:9]
  color_cluster <- pal[ceiling(6*(log(tmp)/max(log(tmp))))]
  wordcloud(words=names(tmp),freq=tmp,scale = c(2, 0),min.freq=1,max.words = 500,
            random.order=F,random.color=F,rot.per=0,colors=color_cluster,ordered.colors=T,
            use.r.layout=F,fixed.asp=F)
  par(fig = c(0,1,0,0.1), mar = c(0, 1, 0, 1), new=TRUE)
  display.brewer.pal(9, "Blues")
}
