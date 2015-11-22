source("utilities.R")

# mission login
getMissionInfo <- function(mission_id){
  #initial emvironment
  conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile")
  #dbListTables(conn)
  res <- dbSendQuery(conn, paste("SELECT * FROM mission_info WHERE mission_id = ",mission_id))
  result <- dbFetch(res,n = -1)
  dbClearResult(res)
  dbDisconnect(conn)
  result
}
#select max(missionid)
maxMissionid <- function(){
  conn <- dbConnect(MySQL(),dbname = "restopicer_user_profile")
  mission_id <-dbSendQuery(conn, paste("SELECT MAX(mission_id) as missionid FROM mission_info "))
  result1 <- dbFetch(mission_id,n = -1)
  dbClearResult(mission_id) 
  dbDisconnect(conn)
  result1
}
#insert intial preference and create new mission
addinitialPrefence<-function(newmission_id,rate1,rate2,rate3,rate4,rate5,rate6,rate7){
  # input$newmission
  conn <- dbConnect(MySQL(),dbname = "restopicer_user_profile")
  dbSendQuery(conn, paste("INSERT INTO mission_info(mission_id,beginning_year,ending_year,round) VALUES ('",newmission_id,"',1975,2014,0 )",sep = ""))
  dbSendQuery(conn, paste("INSERT INTO mission_category_rating(mission_id,sc_code,rating) VALUES ('",newmission_id,"','DI','",rate1,"' )",sep = ""))
  dbSendQuery(conn, paste("INSERT INTO mission_category_rating(mission_id,sc_code,rating) VALUES ('",newmission_id,"','EP','",rate2,"' )",sep = ""))
  dbSendQuery(conn, paste("INSERT INTO mission_category_rating(mission_id,sc_code,rating) VALUES ('",newmission_id,"','ET','",rate3,"' )",sep = ""))
  dbSendQuery(conn, paste("INSERT INTO mission_category_rating(mission_id,sc_code,rating) VALUES ('",newmission_id,"','EW','",rate4,"' )",sep = ""))
  dbSendQuery(conn, paste("INSERT INTO mission_category_rating(mission_id,sc_code,rating) VALUES ('",newmission_id,"','NU','",rate5,"' )",sep = ""))
  dbSendQuery(conn, paste("INSERT INTO mission_category_rating(mission_id,sc_code,rating) VALUES ('",newmission_id,"','PC','",rate6,"' )",sep = ""))
  dbSendQuery(conn, paste("INSERT INTO mission_category_rating(mission_id,sc_code,rating) VALUES ('",newmission_id,"','PE','",rate7,"' )",sep = ""))
  dbDisconnect(conn)
  
}
# addPreferenceKeyword
addPreferenceKeyword <- function(mission_id,newkeyword){
  #initial emvironment
  conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile")
  #dbListTables(conn)
  dbSendQuery(conn, paste("INSERT INTO preference_keyword(mission_id,keyword) VALUES ('",mission_id,"','",newkeyword,"')",sep = ""))
  dbDisconnect(conn)
}
#delete mission
deleteCurrentMission <-function(mission_id){
  conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile")
  dbSendQuery(conn, paste("DELETE FROM mission_info WHERE mission_id=",mission_id,sep = ""))
  dbSendQuery(conn, paste("DELETE FROM mission_category_rating WHERE mission_id=",mission_id,sep = ""))
  dbDisconnect(conn)
}

# go Recommendation
goRecommendation <- function(mission_id,round){
  # get preference keywords
  conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile")
  #dbListTables(conn)
  # get recmmended papers
  res <- dbSendQuery(conn, paste("SELECT * FROM preference_paper WHERE mission_id = ",mission_id," AND round=",round,sep = ""))
  recommendPapers <- dbFetch(res,n = -1)
  dbClearResult(res)
  # get item_ut_already_list papers
  res <- dbSendQuery(conn, paste("SELECT * FROM preference_paper WHERE mission_id = ",mission_id,sep = ""))
  rated_paper <- dbFetch(res,n = -1)
  dbClearResult(res)
  # get preference_keyword
  res <- dbSendQuery(conn, paste("SELECT * FROM preference_keyword WHERE mission_id = ",mission_id))
  preferencedKeywords <- dbFetch(res,n = -1)
  dbClearResult(res)
  if(any(recommendPapers$rating==-1)){
    # searching elastic search
    result <- searchingByItemUT(recommendPapers$item_ut)
  }else{
    # searching elastic search (relevent_N)
    result_relevent <- searchingByKeywords(keywords = paste(preferencedKeywords$keyword,sep = " ",collapse = " "),item_ut_already_list=rated_paper$item_ut,relevent_N = relevent_N)
    # retrieve by preference and quality (composite_N) and active learning (explore_N)
    result <- exploreRecommend(result_relevent,topic,rated_paper,preference_w,quality_w,composite_N,explore_N)
    # save to mysql
    for(item_ut in result$item_ut){
      dbSendQuery(conn, paste("INSERT INTO preference_paper(mission_id,item_ut,rating,round) VALUES ('",mission_id,"','",item_ut,"',",-1,",",round+1,")",sep = ""))
    }
    dbSendQuery(conn, paste("UPDATE mission_info SET round=",round+1," WHERE mission_id=",mission_id,sep = ""))
  }
  dbDisconnect(conn)
  result
}
#searching on elastic search
# 1975 to 2013
# Article
searchingByKeywords <- function(keywords,relevent_N,item_ut_already_list){
  must_not_body <- paste(lapply(item_ut_already_list, function(item_ut){
    paste('{
              \"query_string\": { \"default_field\": \"paper.item_ut\",\"query\": \"',item_ut,'\"}
           }',sep="",collapse = "")
  }),sep = "",collapse = ",")
  jsonbody <- paste('{
      \"query\": {
        \"bool\": {
          \"must\": 
          [{
            \"range\": {
              \"paper.publication_year\": {
                \"from\": \"1975\",
                \"to\": \"2013\"
              }
            }
          },
          {
            \"query_string\": {
              \"default_field\": \"paper.document_type\",
                  \"query\": \"Article\"
            }
          }],
          \"must_not\":[',must_not_body,'],
          \"should\": 
          [{
            \"query_string\": {
              \"default_field\": \"_all\",
              \"query\": "',keywords,'"
            }
          }]
        }
      },
      \"from\": 0,
      \"size\": ',relevent_N,'
    }',sep = "")
  result <- try(fromJSON(httpPOST(searchLocation,postfields = jsonbody)),silent = T)
  df <- rbind_all(lapply(result$hits$hits,function(x){data.frame(item_ut=x$`_source`["item_ut"],
                                                                 article_title=x$`_source`["article_title"],
                                                                 abstract=x$`_source`["abstract"],
                                                                 magazine=x$`_source`["full_source_title"],
                                                                 volume=x$`_source`["volume"],
                                                                 issue=x$`_source`["issue"],
                                                                 publication_year=x$`_source`["publication_year"],
                                                                 stringsAsFactors = F)}))
  as.data.frame(df)
                    }
searchingByItemUT <- function(papers){
  df <- data.frame()
  for(item_ut in papers){
    x <- try(fromJSON(httpGET(paste(es_location,item_ut,sep = "/"))),silent = T)
    tmp <- data.frame(item_ut=x$`_source`["item_ut"],
                      article_title=x$`_source`["article_title"],
                      abstract=x$`_source`["abstract"],
                      magazine=x$`_source`["full_source_title"],
                      volume=x$`_source`["volume"],
                      issue=x$`_source`["issue"],
                      publication_year=x$`_source`["publication_year"],
                      stringsAsFactors = F)
    df <- rbind(df,tmp)
  }
  as.data.frame(df)
}
#mission_info$mission_id,recommend_paper$item_ut[5],input$rate5
rating <- function(mission_id,item_ut,ratevalue){
  conn <- dbConnect(MySQL(), dbname = "restopicer_user_profile")
  dbSendQuery(conn, paste("UPDATE preference_paper SET rating=",ratevalue," WHERE mission_id=",mission_id," AND item_ut='",item_ut,"'",sep = ""))
  dbDisconnect(conn)
}
exploreRecommend <- function(result_relevent,topic,rated_paper,preference_w,quality_w,composite_N,explore_N){
  result_relevent
}
