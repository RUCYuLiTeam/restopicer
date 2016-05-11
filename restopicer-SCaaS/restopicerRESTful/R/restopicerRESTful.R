##### dependencies #####
# if(!require(RMySQL)){install.packages("RMySQL")}
# if(!require(compiler)){install.packages("compiler")}
# if(!require(RJSONIO)){install.packages("RJSONIO")}
# if(!require(RCurl)){install.packages("RCurl")}
# if(!require(dplyr)){install.packages("dplyr")}
# if(!require(elasticnet)){install.packages("elasticnet")}
# if(!require(topicmodels)){install.packages("topicmodels")}
# if(!require(tm)){install.packages("tm")}
# if(!require(wordcloud)){install.packages("wordcloud")}
# if(!require(data.table)){install.packages("data.table")}
# if(!require(bipartite)){install.packages("bipartite")}
# if(!require(igraph)){install.packages("igraph")}
# if(!require(linkcomm)){install.packages("linkcomm")}
#if(!require(glmnet)){install.packages("glmnet")}
# relevent_N <- 1000
# composite_N <- 5
# recommendername <- "weightedHybridRecommender"
##### create new mission with unique username #####
createMission <- function(username){
  #initial emvironment
  conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile",db_username,db_pwd,db_hostname)
  #dbListTables(conn)
  res <- dbSendQuery(conn, paste("INSERT INTO mission_info(name) VALUES ('",username,"')",sep = ""))
  dbDisconnect(conn)
}
##### get user's mission info with unique username #####
getMissionInfo <- function(username){
  #initial emvironment
  conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile",db_username,db_pwd,db_hostname)
  #dbListTables(conn)
  res <- dbSendQuery(conn, paste("SELECT * FROM mission_info WHERE name = '",username,"'",sep = ""))
  result <- dbFetch(res,n = -1)
  dbClearResult(res)
  dbDisconnect(conn)
  result
}

##### get user's rated paper by mission id #####
getRatedPaper <- function(mission_id){
  conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile",db_username,db_pwd,db_hostname)
  #dbListTables(conn)
  res <- dbSendQuery(conn, paste("SELECT * FROM preference_paper WHERE rating > 0 AND mission_id = '",mission_id,"'",sep = ""))
  result <- dbFetch(res,n = -1)
  dbClearResult(res)
  dbDisconnect(conn)
  result
}
###getMaxRatedMissionRound
getMaxRatedMissionRound<- function(username){
  result<- getMissionInfo(username)
  mission_id<- result$mission_id
  ratedpaper <- getRatedPaper(mission_id)
  ratedpaper <- ratedpaper[ratedpaper$rating!=-1,]
  if(nrow(ratedpaper)==0){
    result<- 0
    
  }else{
    result <- max(ratedpaper$mission_round)
  }
  result
}

##### get user's current mission info with unique username #####
getCurrentMissionInfo <- function(username){
  result <- getMissionInfo(username)
  result[which.max(result$mission_id),]
}
##### add PreferenceKeyword for current mission #####
addPreferenceKeyword <- function(username,newkeyword){
  currentMission <- getCurrentMissionInfo(username = username)
  mission_id <- currentMission$mission_id
  #initial emvironment
  conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile",db_username,db_pwd,db_hostname)
  #dbListTables(conn)
  dbSendQuery(conn, paste("INSERT INTO preference_keyword(mission_id,keyword) VALUES ('",mission_id,"','",newkeyword,"')",sep = ""))
  dbDisconnect(conn)
}
addPreferenceKeywords <- function(username,newkeywords){
  currentMission <- getCurrentMissionInfo(username = username)
  mission_id <- currentMission$mission_id
  #initial emvironment
  conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile",db_username,db_pwd,db_hostname)
  #dbListTables(conn)
  for(newkeyword in newkeywords){
    dbSendQuery(conn, paste("INSERT INTO preference_keyword(mission_id,keyword) VALUES ('",mission_id,"','",newkeyword,"')",sep = ""))
  }
  dbDisconnect(conn)
}
##### get history recommendations #####
getAllRatedPaper <- historyrecommendation <- function(username){
  currentMission <- getCurrentMissionInfo(username=username)
  mission_id <- currentMission$mission_id
  mission_round <- currentMission$mission_round
  if(mission_round == 0){
    result <- 0
  }else{
    #get connect
    conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile",db_username,db_pwd,db_hostname)
    #dbListTables
    #get all recommended papers
    res <- dbSendQuery(conn,  paste("SELECT * FROM preference_paper WHERE mission_id = ",mission_id,sep = ""))
    recommendedPapers <- dbFetch(res, n= -1)
    #recommendedPapers <- recommendedPapers[recommendedPapers$rating != -1,]
    dbClearResult(res)
    result <- searchingByItemUT(recommendedPapers[,"item_ut"]) 
    for(i in 1:length(result)){
      result_title <- result[[i]]$item_ut
      # get rec_score
      weightedHybrid_true <- recommendedPapers[which(recommendedPapers$item_ut==result_title),"rec_score_true"]
      #add rec_score to result
      result[[i]]$weightedHybrid_true <- weightedHybrid_true
      #result[[i]] <- list(result[[i]]$article_title,result[[i]]$magazine,result[[i]]$issue,result[[i]]$publication_year,result[[i]]$weightedHybrid) 
    }
    dbDisconnect(conn)
  }
  result
}
#### get similar preference papers
getSimilarpapers <- function(username, relevent=10,searchingControll="exploreHybridRecommend"){
  currentMission <- getCurrentMissionInfo(username = username)
  mission_id <- currentMission$mission_id
  mission_round <- currentMission$mission_round
  # get connect
  conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile",db_username,db_pwd,db_hostname)
  #dbListTables(conn)
  # get All recmmended papers
  res <- dbSendQuery(conn, paste("SELECT * FROM preference_paper WHERE mission_id = ",mission_id,sep = ""))
  recommendedPapers <- dbFetch(res,n = -1)
  dbClearResult(res)
  
  res <- dbSendQuery(conn, paste("SELECT * FROM preference_keyword WHERE mission_id = ",mission_id))
  preferenceKeywords <- dbFetch(res,n = -1)
  dbClearResult(res)
  dbDisconnect(conn)
  result_relevent <- searchingByKeywords(item_ut_already_list=recommendedPapers$item_ut,relevent_N = 10,preferenceKeywords=preferenceKeywords,searchingControll=searchingControll)
  result_relevent
}
##### get top relevent 8 keywords #####
getTopKeywords_old <- function(username,showround=0){
  currentMission <- getCurrentMissionInfo(username = username)
  mission_id <- currentMission$mission_id
  #mission_round <- currentMission$mission_round
  if(currentMission$mission_round==1){
    conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile",db_username,db_pwd,db_hostname)
    #dbListTables(conn)
    # get All recmmended papers
    res <- dbSendQuery(conn, paste("SELECT * FROM preference_paper WHERE mission_id = ",mission_id,sep = ""))
    recommendedPapers <- dbFetch(res,n = -1)
    dbClearResult(res)
    # get All preference keywords
    res <- dbSendQuery(conn, paste("SELECT * FROM preference_keyword WHERE mission_id = ",mission_id))
    preferenceKeywords <- dbFetch(res,n = -1)
    dbClearResult(res)
    rated_papers <- recommendedPapers
    # preprocess for rated papers
    result_rated <- searchingByItemUT(papers = rated_papers$item_ut)
    #rated_papers <- recommendedPapers
    # preprocess for rated papers
    #result_rated <- searchingByItemUT(papers = dm)
    #corpus_rated <- preprocess.abstract.corpus(result_lst = result_rated)
    # generate topic by LDA
    #train_doc <- posterior(object = result_LDA_abstarct_VEM$corpus_topic,newdata = corpus_rated)
    rated_id <- unlist(lapply(result_rated, function(x){
      which(pretrain_doc$item_ut==x$item_ut$item_ut)
    }))
    train_doc <- list(topics=pretrain_doc$topics[rated_id,],terms=pretrain_doc$terms)
    # build elastic model and get coef
    enetmodel <- enet(x = I(train_doc$topics[1:5,]),
                      y = rated_papers$rec_score,
                      lambda=0.5,normalize = F,intercept = T)
    coef <- predict.enet(enetmodel, s=0.5, type="coef", mode="fraction")
    tmp <- coef$coefficients %*% train_doc$terms - min(coef$coefficients %*% train_doc$terms)
    tmp <- as.numeric(tmp)
    names(tmp) <- colnames(train_doc$terms)
  }
  else{
    mission_round <- currentMission$mission_round - 1
    # default
    if(showround<1) showround <- mission_round
    if(showround<1) return(NULL)
    # get connect
    conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile",db_username,db_pwd,db_hostname)
    #dbListTables(conn)
    # get All recmmended papers
    res <- dbSendQuery(conn, paste("SELECT * FROM preference_paper WHERE mission_id = ",mission_id,sep = ""))
    recommendedPapers <- dbFetch(res,n = -1)
    dbClearResult(res)
    dbDisconnect(conn)
    rated_papers <- recommendedPapers[recommendedPapers$rating != -1,]
    # preprocess for rated papers
    result_rated <- searchingByItemUT(papers = rated_papers[rated_papers$rating!=-1,"item_ut"])
    #corpus_rated <- preprocess.abstract.corpus(result_lst = result_rated)
    # generate topic by LDA
    #train_doc <- posterior(object = result_LDA_abstarct_VEM$corpus_topic,newdata = corpus_rated)
    rated_id <- unlist(lapply(result_rated, function(x){
      which(pretrain_doc$item_ut==x$item_ut$item_ut)
    }))
    train_doc <- list(topics=pretrain_doc$topics[rated_id,],terms=pretrain_doc$terms)
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
  }
  tmp <- tmp[order(tmp,decreasing = T)][1:8]
  tmp_name<-names(tmp)
  tmp_name
}
getTopKeywords <- function(username,show_k=10,searchingControll="exploreHybridRecommend"){
  currentMission <- getCurrentMissionInfo(username = username)
  mission_id <- currentMission$mission_id
  mission_round <- currentMission$mission_round
  # get connect
  conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile",db_username,db_pwd,db_hostname)
  # get All recmmended papers
  res <- dbSendQuery(conn, paste("SELECT * FROM preference_paper WHERE mission_id = ",mission_id,sep = ""))
  recommendedPapers <- dbFetch(res,n = -1)
  dbClearResult(res)
  # get All preference keywords
  res <- dbSendQuery(conn, paste("SELECT * FROM preference_keyword WHERE mission_id = ",mission_id))
  preferenceKeywords <- dbFetch(res,n = -1)
  dbClearResult(res)
  # searching elastic search (relevent_N)
  result_relevent <- searchingByKeywords(item_ut_already_list=recommendedPapers$item_ut,relevent_N = 100,preferenceKeywords=preferenceKeywords,searchingControll=searchingControll)
  # using community detection
  relevent_lst <- lapply(result_relevent, function(x){
    if(length(x$keywords$keywords)!=0){ data.frame(item_ut=x$item_ut$item_ut,author_keyword=x$keywords$keywords)}
  })
  papers_keywords_df <- rbindlist(relevent_lst)
  data <- unique(papers_keywords_df)
  bi_matrix <- table(data$item_ut,tolower(data$author_keyword))
  corpus_dtm <- as.DocumentTermMatrix(bi_matrix,weighting = weightTf)
  bi_graph <- graph_from_incidence_matrix(corpus_dtm)
  proj_graph <- bipartite_projection(bi_graph, types = NULL, multiplicity = TRUE,probe1 = NULL, which = "true", remove.type = TRUE)
  coterm_graph <- simplify(proj_graph)
  fc <- fastgreedy.community(coterm_graph)
  community_member_list <- communities(fc)
  if(length(community_member_list)<show_k) {
    fc$membership<-cutat(fc,no=show_k)
    community_member_list <- communities(fc)
  }
  # get show K community
  community_member_list <- community_member_list[order(sapply(community_member_list,function(x){length(x)}),decreasing = T)[1:show_k]]
  # cal weight
  bi_matrix <- matrix(data = 0,nrow = length(community_member_list),ncol = length(V(coterm_graph)),dimnames = c(list(1:length(community_member_list)),list(V(coterm_graph)$name)))
  for(i in 1:length(community_member_list)){
    g <- delete.vertices(coterm_graph,names(V(coterm_graph))[!(names(V(coterm_graph)) %in% community_member_list[[i]])])
    w <- eigen_centrality(g)$vector/sum(eigen_centrality(g)$vector)
    bi_matrix[i,names(V(g))] <- w
  }
  bi_matrix <- as.matrix(bi_matrix)
  as.character(  apply(bi_matrix,1,FUN = function(x){
    tmp <- x[!(names(x) %in% preferenceKeywords$keyword)]
    tmp <- tmp[nchar(names(tmp))>=10 & nchar(names(tmp))<30]
    names(sort(tmp,decreasing = T)[1])
  }))
}
##### goRecommendation for current mission #####
goRecommendation <- function(username,relevent_N=50,recommendername="exploreHybridRecommend",composite_N=5,show_k=10,controllername = "simpleHybridWeightControl",searchingControll="exploreHybridRecommend",...){
  currentMission <- getCurrentMissionInfo(username = username)
  algorithm <- currentMission$algorithm
  mission_id <- currentMission$mission_id
  algorithm <- currentMission$algorithm
  mission_round <- currentMission$mission_round
  
  # get connect
  conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile",db_username,db_pwd,db_hostname)
  #dbListTables(conn)
  # get All recmmended papers
  res <- dbSendQuery(conn, paste("SELECT * FROM preference_paper WHERE mission_id = ",mission_id,sep = ""))
  recommendedPapers <- dbFetch(res,n = -1)
  dbClearResult(res)
  # get All preference keywords
  res <- dbSendQuery(conn, paste("SELECT * FROM preference_keyword WHERE mission_id = ",mission_id))
  preferenceKeywords <- dbFetch(res,n = -1)
  dbClearResult(res)
  # get All preference topic
  res <- dbSendQuery(conn, paste("SELECT * FROM preference_topic WHERE mission_id = ",mission_id,sep = ""))
  dropped_topic <- dbFetch(res,n = -1)
  dbClearResult(res)

  # searching elastic search (relevent_N)
  result_relevent <- searchingByKeywords(item_ut_already_list=recommendedPapers[recommendedPapers$rating!=-1,]$item_ut,relevent_N = relevent_N*3,preferenceKeywords=preferenceKeywords,searchingControll=searchingControll)
  # using community detection
  relevent_lst <- lapply(result_relevent, function(x){
    if(length(x$keywords$keywords) !=0){data.frame(item_ut=x$item_ut$item_ut,author_keyword=x$keywords$keywords)}
    
  })
  papers_keywords_df <- rbindlist(relevent_lst)
  data <- unique(papers_keywords_df)
  bi_matrix <- table(data$item_ut,tolower(data$author_keyword))
  corpus_dtm <- as.DocumentTermMatrix(bi_matrix,weighting = weightTf)
  bi_graph <- graph_from_incidence_matrix(corpus_dtm)
  proj_graph <- bipartite_projection(bi_graph, types = NULL, multiplicity = TRUE,probe1 = NULL, which = "true", remove.type = TRUE)
  coterm_graph <- simplify(proj_graph)
  fc <- fastgreedy.community(coterm_graph)
  community_member_list <- communities(fc)
  if(length(community_member_list)<show_k) {
    fc$membership<-cutat(fc,no=show_k)
    community_member_list <- communities(fc)
  }
  # get show K community
  community_member_list <- community_member_list[order(sapply(community_member_list,function(x){length(x)}),decreasing = T)[1:show_k]]
  # cal weight
  bi_matrix <- matrix(data = 0,nrow = length(community_member_list),ncol = length(V(coterm_graph)),dimnames = c(list(1:length(community_member_list)),list(V(coterm_graph)$name)))
  for(i in 1:length(community_member_list)){
    g <- delete.vertices(coterm_graph,names(V(coterm_graph))[!(names(V(coterm_graph)) %in% community_member_list[[i]])])
    w <- eigen_centrality(g)$vector/sum(eigen_centrality(g)$vector)
    bi_matrix[i,names(V(g))] <- w
  }
  bi_matrix <- as.matrix(bi_matrix)
  top_keywords <- as.character(  apply(bi_matrix,1,FUN = function(x){
    tmp <- x[!(names(x) %in% preferenceKeywords$keyword)]
    tmp <- tmp[nchar(names(tmp))>=10 & nchar(names(tmp))<30]
    names(sort(tmp,decreasing = T)[1])
  }))
  
    if(any(recommendedPapers$rating <= 0)){
      # searching elastic search
      result <- searchingByItemUT(recommendedPapers[recommendedPapers$rating==-1,"item_ut"])    
      for(i in 1:length(result)){
        result_title <- result[[i]]$item_ut
        #add rec_score to result
        result[[i]]$mission_round <- recommendedPapers[which(recommendedPapers$item_ut==result_title),"mission_round"]
        result[[i]]$exploration_w <- recommendedPapers[which(recommendedPapers$item_ut==result_title),"exploration_w"]
        result[[i]]$weightedHybrid_true <- recommendedPapers[which(recommendedPapers$item_ut==result_title),"rec_score_true"]
        result[[i]]$relevent <-  recommendedPapers[which(recommendedPapers$item_ut==result_title),"relevent"]
        result[[i]]$pred_rating <-  recommendedPapers[which(recommendedPapers$item_ut==result_title),"pred_rating"]
        result[[i]]$quality <-  recommendedPapers[which(recommendedPapers$item_ut==result_title),"quality"]
        result[[i]]$learn_ability <-  recommendedPapers[which(recommendedPapers$item_ut==result_title),"learn_ability"]
        result[[i]]$summary_degree <-  recommendedPapers[which(recommendedPapers$item_ut==result_title),"summary_degree"]
        result[[i]]$fresh <-  recommendedPapers[which(recommendedPapers$item_ut==result_title),"fresh"]
      }
    }else{
      #judge if user like recommended papers
      if(mission_round == 0){
        if_like=0
      }else if(nrow(recommendedPapers)==5){
        current_round_rating <-  sum(recommendedPapers$rating[(nrow(recommendedPapers)-4):nrow(recommendedPapers)])/5
        if(current_round_rating >=3){
          if_like =1
        }else {
          if_like =0
        }
      }else if(nrow(recommendedPapers)>5){
        current_round_rating <-  sum(recommendedPapers$rating[(nrow(recommendedPapers)-4):nrow(recommendedPapers)])/5
        previous_round_rating <- sum(recommendedPapers$rating[-c((nrow(recommendedPapers)-4):nrow(recommendedPapers))])/(nrow(recommendedPapers)-5)
        if(current_round_rating >= previous_round_rating){
          if_like =1
        }else {
          if_like =0
        }
      }
      # searching elastic search (relevent_N)
      result_relevent <- searchingByKeywords(item_ut_already_list=recommendedPapers$item_ut,relevent_N = relevent_N,preferenceKeywords=preferenceKeywords,searchingControll=searchingControll)
      # retrieve by recommender (composite_N)
      doRecommend <- getRecommender(recommendername = recommendername)
      mission_round <- mission_round + 1
      result <- doRecommend(result_relevent=result_relevent,rated_papers=recommendedPapers,composite_N=composite_N,mission_round=mission_round,dropped_topic=dropped_topic,controllername=controllername,if_like=if_like)
      # save to mysql
      for(tmp in result){
        item_ut <- tmp$item_ut$item_ut
        #rec_score<- tmp$weightedHybrid
        relevent<- tmp$relevent
        pred_rating<- tmp$pred_rating
        quality<- tmp$quality
        learn_ability<- tmp$learn_ability
        summary_degree <- tmp$summary_degree
        fresh <- tmp$fresh
        rec_score_true <- tmp$weightedHybrid_true
        exploration_w <- tmp$exploration_w
        dbSendQuery(conn, paste("INSERT INTO preference_paper(mission_id,item_ut,rating,mission_round,exploration_w,relevent,pred_rating,quality,learn_ability,summary_degree,fresh,rec_score_true) 
                                VALUES ('",mission_id,"','",item_ut,"',",-1,",",mission_round,",",exploration_w,",",
                                relevent,",",pred_rating,",",quality,",",learn_ability,",",summary_degree,",",fresh,",",rec_score_true,")",sep = ""))
      }
      dbSendQuery(conn, paste("UPDATE mission_info SET mission_round=",mission_round," WHERE mission_id=",mission_id,sep = ""))
      # plot wordcloud
      image_name <- paste(mission_round ,".jpg",sep="")
      #path <- "E:/phpStudy/WWW/restopicer/sites/all/modules/custom/restopicer/images"
      path <- restopicer_pic_path
      filename <- paste(path,username,sep="/")
      dir.create(filename)
      if(!file.exists(paste(filename,image_name,sep ="/"))){
        if(mission_round==1){
          # get All recmmended papers
          res <- dbSendQuery(conn, paste("SELECT * FROM preference_paper WHERE mission_id = ",mission_id,sep = ""))
          recommendedPapers_1 <- dbFetch(res,n = -1)
          dbClearResult(res)
          rated_papers <- recommendedPapers_1
          # preprocess for rated papers
          result_rated <- searchingByItemUT(papers = rated_papers$item_ut)
          #rated_papers <- recommendedPapers
          # preprocess for rated papers
          #result_rated <- searchingByItemUT(papers = dm)
          #corpus_rated <- preprocess.abstract.corpus(result_lst = result_rated)
          # generate topic by LDA
          #train_doc <- posterior(object = result_LDA_abstarct_VEM$corpus_topic,newdata = corpus_rated)
#           rated_id <- unlist(lapply(result_rated, function(x){
#             which(pretrain_doc$item_ut==x$item_ut$item_ut)
#           }))
#           train_doc <- list(topics=pretrain_doc$topics[rated_id,],terms=pretrain_doc$terms)
#           # build elastic model and get coef
#           enetmodel <- enet(x = I(train_doc$topics[1:5,]),
#                             y = rated_papers$rec_score_true,
#                             lambda=0.5,normalize = F,intercept = T)
#           coef <- predict.enet(enetmodel, s=0.5, type="coef", mode="fraction")
#           tmp <- coef$coefficients %*% train_doc$terms - min(coef$coefficients %*% train_doc$terms)
#           tmp <- as.numeric(tmp)
#           names(tmp) <- colnames(train_doc$terms)
#           tmp<-tmp[order(tmp,decreasing = T)][1:100]
#           i <- seq(from = 2, to = 0.01,by=-0.02)
#           tmp[1:100] <- seq(from = 100, to = 1,by=-1)
#           tmp <- tmp*i
#           tmp[1] <- 300
          res_keywords <- c()
          kw <- c()
          for(i in 1:length(result_rated)){
            result_title <- result_rated[[i]]$item_ut
            #add rec_score to result
            result_rated[[i]]$rated <- recommendedPapers[which(recommendedPapers$item_ut==result_title),"rec_score_true"]
            result_rated[[i]]$rated <- round(result_rated[[i]]$rated)
            res_keywords <- c(res_keywords,table(result_rated[[i]]$keywords)*result_rated[[i]]$rated)
          }
          res_keywords <- as.matrix(res_keywords)
          kw_freq<- matrix(nrow = nrow(res_keywords),ncol = 2)
          kw_freq[,1]=dimnames(res_keywords)[[1]]
          kw_freq[,2]=res_keywords[,1] 
          colnames(kw_freq) = c("keywords","freq")
          kw_freq[,1]=tolower(kw_freq[,1])
          kw <- as.matrix(which(table(kw_freq[,1])>1))
          k <- dimnames(kw)[[1]]
          k_len <- length(k)
          len <- c(1:K_len)
          k_ids <- c()
          for(i in 1:k_len){
            len[i] <- sum(as.numeric(kw_freq[which(kw_freq[,1]==k[i]),2]))
            k_id <- which(kw_freq[,1]==k[i])
            k_ids <- c(k_ids,k_id)
          }
          kw_fre_1 <- kw_freq[-k_ids,]
          kw_fre_2 <- cbind(k,len)
          kw_fre_new <- rbind(kw_fre_1,kw_fre_2)
          kw_fre_new[,2] <- as.numeric(kw_fre_new[,2])
          kw_fre_new<-kw_fre_new[order(as.numeric(kw_fre_new[,2]),decreasing = T),]
          i <- seq(from = 2, to = 0.01,by=-2/nrow(kw_fre_new))
          kw_fre_new[,2] <- seq(from=nrow(kw_fre_new), to=1, by=-1)
          kw_fre_new[,2] <- as.numeric(kw_fre_new[,2])
          kw_fre_new[,2] <- as.numeric(kw_fre_new[,2])*i
          kw_fre_new[1,2] <- as.numeric(kw_fre_new[1,2]) + mean(as.numeric(kw_fre_new[,2]))

          
          round_name <- paste( mission_round,".jpg",sep="")
          png(file.path(path,paste(username , round_name ,sep="/")))
          par(fig = c(0,1,0,1),mar = c(0,0,0,0))
          #pal <- brewer.pal(9,"Blues")[4:9]
          #color_cluster <- pal[ceiling(6*(log(tmp)/max(log(tmp))))]
          wordcloud(words=kw_fre_new[,1],freq=as.numeric(kw_fre_new[,2]),scale = c(4, 0.5),max.words = 100,
                    random.order=F,random.color=F,colors=rainbow(100),ordered.colors=F,
                    use.r.layout=F)
          dev.off()
        }else{         
          rated_papers <- recommendedPapers[recommendedPapers$rating != -1,]
          # preprocess for rated papers
          result_rated <- searchingByItemUT(papers = rated_papers[rated_papers$rating!=-1,"item_ut"])
#           #corpus_rated <- preprocess.abstract.corpus(result_lst = result_rated)
#           # generate topic by LDA
#           #train_doc <- posterior(object = result_LDA_abstarct_VEM$corpus_topic,newdata = corpus_rated)
#           rated_id <- unlist(lapply(result_rated, function(x){
#             which(pretrain_doc$item_ut==x$item_ut$item_ut)
#           }))
#           train_doc <- list(topics=pretrain_doc$topics[rated_id,],terms=pretrain_doc$terms)
#           # build elastic model and get coef
#           rated_bool <- (rated_papers$rating!=-1)
#           if(length(unique(rated_papers$rating[rated_bool]))==1){
#             normrating <- rnorm(length(rated_bool), mean = 0, sd = 0.5)
#             rated_papers$rating[rated_bool] <- rated_papers$rating[rated_bool] + normrating
#             #rated_papers$rating[rated_bool] <- rated_papers$rating[rated_bool] - min(rated_papers$rating[rated_bool])
#           }
#           enetmodel <- enet(x = I(train_doc$topics),
#                             y = rated_papers$rating,
#                             lambda=0.5,normalize = F,intercept = T)
#           coef <- predict.enet(enetmodel, s=0.5, type="coef", mode="fraction")
#           tmp <- coef$coefficients %*% train_doc$terms - min(coef$coefficients %*% train_doc$terms)
#           tmp <- as.numeric(tmp)
#           tmp<- tmp/max(tmp)
#           names(tmp) <- colnames(train_doc$terms)
#           tmp<-tmp[order(tmp,decreasing = T)][1:100]
#           i <- seq(from = 2, to = 0.01,by=-0.02)
#           tmp[1:100] <- seq(from = 100, to = 1,by=-1)
#           tmp <- tmp*i
#           tmp[1] <- 300
          res_keywords <- c()
          kw <- c()
          for(i in 1:length(result_rated)){
            result_title <- result_rated[[i]]$item_ut
            #add rec_score to result
            result_rated[[i]]$rated <- recommendedPapers[which(recommendedPapers$item_ut==result_title),"rating"]
            res_keywords <- c(res_keywords,table(result_rated[[i]]$keywords)*result_rated[[i]]$rated)
          }
          res_keywords <- as.matrix(res_keywords)
          kw_freq<- matrix(nrow = nrow(res_keywords),ncol = 2)
          kw_freq[,1]=dimnames(res_keywords)[[1]]
          kw_freq[,2]=res_keywords[,1] 
          colnames(kw_freq) = c("keywords","freq")
          kw_freq[,1]=tolower(kw_freq[,1])
          kw <- as.matrix(which(table(kw_freq[,1])>1))
          k <- dimnames(kw)[[1]]
          k_len <- length(k)
          len <- c(1:K_len)
          k_ids <- c()
          for(i in 1:k_len){
            len[i] <- sum(as.numeric(kw_freq[which(kw_freq[,1]==k[i]),2]))
            k_id <- which(kw_freq[,1]==k[i])
            k_ids <- c(k_ids,k_id)
          }
          kw_fre_1 <- kw_freq[-k_ids,]
          kw_fre_2 <- cbind(k,len)
          kw_fre_new <- rbind(kw_fre_1,kw_fre_2)
          kw_fre_new[,2] <- as.numeric(kw_fre_new[,2])
          kw_fre_new<-kw_fre_new[order(as.numeric(kw_fre_new[,2]),decreasing = T),]
          i <- seq(from = 2, to = 0.01,by=-2/nrow(kw_fre_new))
          kw_fre_new[,2] <- seq(from=nrow(kw_fre_new), to=1, by=-1)
          kw_fre_new[,2] <- as.numeric(kw_fre_new[,2])
          kw_fre_new[,2] <- as.numeric(kw_fre_new[,2])*i
          kw_fre_new[1,2] <- as.numeric(kw_fre_new[1,2]) + mean(as.numeric(kw_fre_new[,2]))
          
          
          round_name <- paste(mission_round,".jpg",sep="")
          
          png(file.path(path,username , round_name))
          
          par(fig = c(0,1,0,1),mar = c(0,0,0,0))
          #pal <- brewer.pal(9,"Blues")[4:9]
          #color_cluster <- pal[ceiling(6*(log(tmp)/max(log(tmp))))]
          wordcloud(words=kw_fre_new[,1],freq=as.numeric(kw_fre_new[,2]),scale = c(3.2, 0.5),max.words = 100,
                    random.order=F,random.color=F,colors=rainbow(100),ordered.colors=F,
                    use.r.layout=F)
          dev.off()
          
        }
      }
    }

  
  dbDisconnect(conn)
  result$mission_id <- mission_id
  result$algorithm <- algorithm
  result$username <- username
  result$top_keywords <- top_keywords
  result
  }
##### possible research direction
possible_direction <- function(username,relevent_N=50,recommendername="preferenceOnlyRecommend",composite_N=5,show_k=10,searchingControll="possibledirection",...){
  currentMission <- getCurrentMissionInfo(username = username)
  mission_id <- currentMission$mission_id
  mission_round <- currentMission$mission_round
  
  # get connect
  conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile",db_username,db_pwd,db_hostname)
  #dbListTables(conn)
  # get All recmmended papers
  res <- dbSendQuery(conn, paste("SELECT * FROM preference_paper WHERE mission_id = ",mission_id,sep = ""))
  recommendedPapers <- dbFetch(res,n = -1)
  dbClearResult(res)
  # get All preference keywords
  res <- dbSendQuery(conn, paste("SELECT * FROM preference_keyword WHERE mission_id = ",mission_id))
  preferenceKeywords <- dbFetch(res,n = -1)
  dbClearResult(res)
  # get All preference topic
  res <- dbSendQuery(conn, paste("SELECT * FROM preference_topic WHERE mission_id = ",mission_id,sep = ""))
  dropped_topic <- dbFetch(res,n = -1)
  dbClearResult(res)
  # searching elastic search (relevent_N)
  result_relevent <- searchingByKeywords(item_ut_already_list=recommendedPapers$item_ut,relevent_N = relevent_N,preferenceKeywords=preferenceKeywords,searchingControll=searchingControll)
  # using community detection
  relevent_lst <- lapply(result_relevent, function(x){
    if(length(x$keywords$keywords) !=0){data.frame(item_ut=x$item_ut$item_ut,author_keyword=x$keywords$keywords)}
    
  })
  papers_keywords_df <- rbindlist(relevent_lst)
  data <- unique(papers_keywords_df)
  bi_matrix <- table(data$item_ut,tolower(data$author_keyword))
  corpus_dtm <- as.DocumentTermMatrix(bi_matrix,weighting = weightTf)
  bi_graph <- graph_from_incidence_matrix(corpus_dtm)
  proj_graph <- bipartite_projection(bi_graph, types = NULL, multiplicity = TRUE,probe1 = NULL, which = "true", remove.type = TRUE)
  coterm_graph <- simplify(proj_graph)
  fc <- fastgreedy.community(coterm_graph)
  community_member_list <- communities(fc)
  if(length(community_member_list)<show_k) {
    fc$membership<-cutat(fc,no=show_k)
    community_member_list <- communities(fc)
  }
  # get show K community
  community_member_list <- community_member_list[order(sapply(community_member_list,function(x){length(x)}),decreasing = T)[1:show_k]]
  # cal weight
  bi_matrix <- matrix(data = 0,nrow = length(community_member_list),ncol = length(V(coterm_graph)),dimnames = c(list(1:length(community_member_list)),list(V(coterm_graph)$name)))
  for(i in 1:length(community_member_list)){
    g <- delete.vertices(coterm_graph,names(V(coterm_graph))[!(names(V(coterm_graph)) %in% community_member_list[[i]])])
    w <- eigen_centrality(g)$vector/sum(eigen_centrality(g)$vector)
    bi_matrix[i,names(V(g))] <- w
  }
  bi_matrix <- as.matrix(bi_matrix)
  top_keywords <- as.character(  apply(bi_matrix,1,FUN = function(x){
    tmp <- x[!(names(x) %in% preferenceKeywords$keyword)]
    tmp <- tmp[nchar(names(tmp))>=10 & nchar(names(tmp))<30]
    names(sort(tmp,decreasing = T)[1])
  }))
  
  res <- dbSendQuery(conn, paste("SELECT * FROM possible_direction WHERE mission_id = ",mission_id))
  posDirection <- dbFetch(res,n = -1)
  dbClearResult(res)
  if(nrow(posDirection) > 0){
    # searching elastic search
    result <- searchingByItemUT(posDirection$item_ut)    
    for(i in 1:length(result)){
      result_title <- result[[i]]$item_ut
      #add rec_score to result
      result[[i]]$weightedHybrid <- posDirection[which(posDirection$item_ut==result_title),"rec_score"]
      result[[i]]$relevent <-  posDirection[which(posDirection$item_ut==result_title),"relevent"]
      result[[i]]$pred_rating <-  posDirection[which(posDirection$item_ut==result_title),"pred_rating"]
    }
  }else{
    # searching elastic search (relevent_N)
    result_relevent <- searchingByKeywords(item_ut_already_list=recommendedPapers$item_ut,relevent_N = relevent_N,preferenceKeywords=preferenceKeywords,searchingControll=searchingControll)
    # retrieve by recommender (composite_N)
    doRecommend <- getRecommender(recommendername = recommendername)
    result <- doRecommend(result_relevent=result_relevent,rated_papers=recommendedPapers[which(recommendedPapers$rating!=-1),],composite_N=composite_N,mission_round=mission_round,dropped_topic=dropped_topic)
    
    for(tmp in result){
      item_ut <- tmp$item_ut$item_ut
      rec_score<- tmp$weightedHybrid
      relevent<- tmp$relevent
      pred_rating<- tmp$pred_rating
      dbSendQuery(conn, paste("INSERT INTO possible_direction(mission_id,item_ut,rec_score,relevent,pred_rating) 
                              VALUES ('",mission_id,"','",item_ut,"',",rec_score,",",
                              relevent,",",pred_rating,")",sep = ""))
    }
  }
  dbDisconnect(conn)
  # topic_show <- topic_show(username)
  result$username <- username
  result$top_keywords <- top_keywords
  result$topic_show <- topic_show(username)
  result
  
  }

##### rating papers current mission of unique username #####
rating <- function(username,item_ut,ratevalue){
  currentMission <- getCurrentMissionInfo(username = username)
  mission_id <- currentMission$mission_id
  conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile",db_username,db_pwd,db_hostname)
  dbSendQuery(conn, paste("UPDATE preference_paper SET rating=",ratevalue," WHERE mission_id=",mission_id," AND item_ut='",item_ut,"'",sep = ""))
  dbDisconnect(conn)
}
ratings <- function(username,item_ut_array,ratevalues){
  currentMission <- getCurrentMissionInfo(username = username)
  mission_id <- currentMission$mission_id
  if(length(item_ut_array)==length(ratevalues)){
    conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile")
    for(i in 1: length(ratevalues)){
      item_ut <- item_ut_array[i]
      ratevalue <- ratevalues[i]
      dbSendQuery(conn, paste("UPDATE preference_paper SET rating=",ratevalue," WHERE mission_id=",mission_id," AND item_ut='",item_ut,"'",sep = ""))
    }
    dbDisconnect(conn)
  }
}
##### select topic that user liked and disliked (which suggested removed) ##### 
topic_show <- function(username){
  currentMission <- getCurrentMissionInfo(username = username)
  mission_id <- currentMission$mission_id
  mission_round <- currentMission$mission_round
  if(mission_round == 0){
    top_topics <- 0
    bottom_topics <- 0
  }else{
    conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile",db_username,db_pwd,db_hostname)
    #dbListTables(conn)
    # get All recmmended papers
    res <- dbSendQuery(conn, paste("SELECT * FROM preference_paper WHERE mission_id = ",mission_id,sep = ""))
    recommendedPapers <- dbFetch(res,n = -1)
    dbClearResult(res)
    # get All preference topic
    res <- dbSendQuery(conn, paste("SELECT * FROM preference_topic WHERE mission_id = ",mission_id,sep = ""))
    dropped_topic <- dbFetch(res,n = -1)
    dbClearResult(res)
    # get rated paper
    rated_papers <- recommendedPapers[recommendedPapers$rating != -1,]
    if(nrow(rated_papers)==0){
      rated_papers <- recommendedPapers
      # preprocess for rated papers
      result_rated <- searchingByItemUT(papers = rated_papers$item_ut)
      # preprocess for rated papers
      #corpus_rated <- preprocess.abstract.corpus(result_lst = result_rated)
      # generate topic by LDA
      #train_doc <- posterior(object = result_LDA_abstarct_VEM$corpus_topic,newdata = corpus_rated)
      rated_id <- unlist(lapply(result_rated, function(x){
        which(pretrain_doc$item_ut==x$item_ut$item_ut)
      }))
      train_doc <- list(topics=pretrain_doc$topics[rated_id,],terms=pretrain_doc$terms)
      #dropped topics for enet
      if(length(dropped_topic$topic_drop)!=0)  train_doc$topics <- train_doc$topics[,-dropped_topic$topic_drop]
      # build elastic model and get coef
      enetmodel <- enet(x = I(train_doc$topics),
                        y = rated_papers$rec_score,
                        lambda=0.5,normalize = F,intercept = T)
    }else{
      # preprocess for rated papers
      result_rated <- searchingByItemUT(papers = rated_papers$item_ut)
      # preprocess for rated papers
      #corpus_rated <- preprocess.abstract.corpus(result_lst = result_rated)
      # generate topic by LDA
      #train_doc <- posterior(object = result_LDA_abstarct_VEM$corpus_topic,newdata = corpus_rated)
      rated_id <- unlist(lapply(result_rated, function(x){
        which(pretrain_doc$item_ut==x$item_ut$item_ut)
      }))
      train_doc <- list(topics=pretrain_doc$topics[rated_id,],terms=pretrain_doc$terms)
      #dropped topics for enet
      if(length(dropped_topic$topic_drop)!=0)  train_doc$topics <- train_doc$topics[,-dropped_topic$topic_drop]
      # build elastic model and get coef
      enetmodel <- enet(x = I(train_doc$topics),
                        y = rated_papers$rating,
                        lambda=0.5,normalize = F,intercept = T)
    }
    coef <- predict.enet(enetmodel, s=0.5, type="coef", mode="fraction")
    top_topics <- names(sort(coef$coefficients)[(length(coef$coefficients)-5):length(coef$coefficients)])
    bottom_topics <- names(sort(coef$coefficients)[1:6])
    
  }
  list(top_topics=top_topics,bottom_topics=bottom_topics)
}
##### clear current mission of unique username #####
clearMission <- createMission
