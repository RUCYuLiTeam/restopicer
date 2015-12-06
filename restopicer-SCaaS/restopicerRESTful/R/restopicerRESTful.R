##### dependencies #####
if(!require(RMySQL)){install.packages("RMySQL")}
if(!require(compiler)){install.packages("compiler")}
##### create new mission with unique username #####
createMission <- function(username){
  #initial emvironment
  conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile")
  #dbListTables(conn)
  res <- dbSendQuery(conn, paste("INSERT INTO mission_info(name) VALUES ('",username,"')",sep = ""))
  dbDisconnect(conn)
}
##### get user's mission info with unique username #####
getMissionInfo <- function(username){
  #initial emvironment
  conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile")
  #dbListTables(conn)
  res <- dbSendQuery(conn, paste("SELECT * FROM mission_info WHERE name = '",username,"'",sep = ""))
  result <- dbFetch(res,n = -1)
  dbClearResult(res)
  dbDisconnect(conn)
  result
}
##### get user's current mission info with unique username #####
getCurrentMissionInfo <- function(username){
  result <- getMissionInfo(username)
  result[which.max(result$mission_id),]
}
##### addPreferenceKeyword for current mission #####
addPreferenceKeyword <- function(username,newkeyword){
  currentMission <- getCurrentMissionInfo(username = username)
  mission_id <- currentMission$mission_id
  #initial emvironment
  conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile")
  #dbListTables(conn)
  dbSendQuery(conn, paste("INSERT INTO preference_keyword(mission_id,keyword) VALUES ('",mission_id,"','",newkeyword,"')",sep = ""))
  dbDisconnect(conn)
}
addPreferenceKeywords <- function(username,newkeywords){
  currentMission <- getCurrentMissionInfo(username = username)
  mission_id <- currentMission$mission_id
  #initial emvironment
  conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile")
  #dbListTables(conn)
  for(newkeyword in newkeywords){
    dbSendQuery(conn, paste("INSERT INTO preference_keyword(mission_id,keyword) VALUES ('",mission_id,"','",newkeyword,"')",sep = ""))
  }
  dbDisconnect(conn)
}
##### goRecommendation for current mission #####
getRecommender<-function(recommendername){
  cmpfun(
    switch(recommendername,
           hybridRecommender=hybridRecommend
    ))
}
goRecommendation <- function(username,relevent_N,recommendername="hybridRecommender",
                             hybrid_N=relevent_N,
                             preference_w=1,quality_w=1,summary_w=1,fresh_w=1,explore_w=1,
                             topics_filepath=""){
  currentMission <- getCurrentMissionInfo(username = username)
  mission_id <- currentMission$mission_id
  mission_round <- currentMission$mission_round
  # get preference keywords
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
  if(any(recommendedPapers$rating==-1)){
    # searching elastic search
    result <- searchingByItemUT(recommendedPapers[recommendedPapers$rating==-1,"item_ut"])
  }else{
    # searching elastic search (relevent_N)
    result_relevent <- searchingByKeywords(keywords = paste(preferencedKeywords$keyword,sep = " ",collapse = " "),item_ut_already_list=recommendedPapers$item_ut,relevent_N = relevent_N)
    # retrieve by preference and quality (composite_N) and active learning (explore_N)
    doRecommend<- getRecommender(recommendername = recommendername)
    result <- doRecommend(result_relevent=result_relevent,
                              topics_filepath=topics_filepath,rated_papers=recommendedPapers,
                              preference_w=preference_w,quality_w=quality_w,summary_w=summary_w,fresh_w=fresh_w,explore_w=explore_w,
                              hybrid_N=hybrid_N)
    mission_round <- mission_round + 1
    # save to mysql
    for(item_ut in result$item_ut){
      dbSendQuery(conn, paste("INSERT INTO preference_paper(mission_id,item_ut,rating,mission_round) VALUES ('",mission_id,"','",item_ut,"',",-1,",",mission_round,")",sep = ""))
    }
    dbSendQuery(conn, paste("UPDATE mission_info SET mission_round=",mission_round," WHERE mission_id=",mission_id,sep = ""))
  }
  dbDisconnect(conn)
  result
}
##### rating papers current mission of unique username #####
rating <- function(username,item_ut,ratevalue){
  currentMission <- getCurrentMissionInfo(username = username)
  mission_id <- currentMission$mission_id
  conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile")
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
##### clear current mission of unique username #####
clearMission <- createMission