#require(RJSONIO)
#require(RCurl)
#require(dplyr)

elastic_search_server <- "http://127.0.0.1:9200"
indexname <- "restopicer"
typename  <- "paper"
es_location <- paste(elastic_search_server,indexname,typename,sep = "/")
searchLocation <- paste(es_location,"_search",sep = "/")

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