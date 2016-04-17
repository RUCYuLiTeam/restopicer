elastic_search_server <- "http://127.0.0.1:9200"
indexname <- "restopicer"
typename  <- "paper"
es_location <- paste(elastic_search_server,indexname,typename,sep = "/")
searchLocation <- paste(es_location,"_search",sep = "/")
#searching on elastic search
# 1975 to 2013
# Article
searchingByKeywords <- function(keywords,relevent_N,item_ut_already_list,preferenceKeywords){
  query_must <- preferenceKeywords[which.max(preferenceKeywords$id),"keyword"]
  query_should <- paste(preferenceKeywords[-which.max(preferenceKeywords$id),"keyword"],sep = " ",collapse = " ")
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
                  \"to\": \"2014\"
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
                  \"multi_match\": {
                    \"query\": "',query_must,'",
                    \"type\": \"best_fields\",
                    \"fields\": [ \"paper.article_title^5\",\"paper.abstract^2\",\"paper.keywords^3\" ],
                    \"operator\": \"and\",
                    \"tie_breaker\": 0.3,
                    \"boost\": 3
              }
            },
            {
              \"multi_match\": {
                \"query\": "',query_should,'",
                \"type\": \"best_fields\",
                \"fields\": [ \"paper.article_title^5\",\"paper.abstract^2\",\"paper.keywords^3\" ],
                \"operator\": \"or\",
                \"tie_breaker\": 0.3,
                \"boost\": 1
              }
            }]
          }
        },
        \"from\": 0,
        \"size\": ',relevent_N,'
      }',sep = "")
  result <- try(fromJSON(httpPOST(searchLocation,postfields = jsonbody)),silent = T)
  result_list <- lapply(result$hits$hits,function(x){
    list(item_ut=x$`_source`["item_ut"],
         article_title=x$`_source`["article_title"],
         abstract=x$`_source`["abstract"],
         magazine=x$`_source`["full_source_title"],
         volume=x$`_source`["volume"],
         issue=x$`_source`["issue"],
         publication_year=x$`_source`["publication_year"],
         authors=x$`_source`["authors"],
         keywords=x$`_source`["keywords"],
         score=x$`_score`)
  })
  result_list
}

searchingByItemUT <- function(papers){
  result_list <- NULL
  for(item_ut in papers){
    x <- try(fromJSON(httpGET(paste(es_location,item_ut,sep = "/"))),silent = T)
    tmp <- list(item_ut=x$`_source`["item_ut"],
                article_title=x$`_source`["article_title"],
                abstract=x$`_source`["abstract"],
                magazine=x$`_source`["full_source_title"],
                volume=x$`_source`["volume"],
                issue=x$`_source`["issue"],
                publication_year=x$`_source`["publication_year"],
                authors=x$`_source`["authors"],
                keywords=x$`_source`["keywords"])
    if(is.null(result_list)){
      result_list <- list(tmp)
    }else{
      result_list <- append(result_list,list(tmp))
    }
  }
  result_list
}