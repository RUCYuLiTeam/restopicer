###########
# convert DataFrame to SQL data file
###########
convertDataFrametoInsertSQLFile <- function(table_name,data){
  unlist(
    alply(data,.margins = 1,function(r){
      paste("INSERT INTO",table_name,"(",paste(colnames(r),collapse = ","),")","VALUES","(",paste("\"",r,"\"",sep = "",collapse = ","),")",sep = " ")
    })
  )
}
###########
# convert Dimension To Attribute, just like reshape (but reshape has limitations)
###########
convertDimensionToAttribute<-function(data,dimensions,values,DIMENSION_NAME="ATTRIBUTE",VALUE_NAME="VALUE"){
  head(data$UNIQUE<-c(1:nrow(data)),1)
  head(rownames(data)<-c(1:nrow(data)),1)
  head(AttributeWithValue<-as.data.frame(
    as.table(
      as.matrix(
        data[,values]
      ))),5)
  head(colnames(AttributeWithValue)<-c("UNIQUE",DIMENSION_NAME,VALUE_NAME),1)
  #library(plyr)
  system.time(result <- join(data[,append(dimensions,ncol(data))], AttributeWithValue, by = c("UNIQUE")))
  result$UNIQUE<-NULL
  head(result)
  return(result)
}
###########
# utilities for data
###########
constantWeightImpact <- function(x){
  ifelse(x>0,1,0)
}
linearWeightImpact <- function(x){
  x
}
quadWeightImpact <- function(x){
  x^2
}
###########
# memory control (exec last)
###########
addPersistentObjects <- function(names){
  memoryWhiteList <<- unique(c(memoryWhiteList,names))
}
rmTempObject <- function(){
  rm(list = setdiff(ls(envir = globalenv()), memoryWhiteList),envir = globalenv())
  gc()
  #ls(envir = globalenv())
}
memoryWhiteList <- c("memoryWhiteList",ls(envir = globalenv()))
