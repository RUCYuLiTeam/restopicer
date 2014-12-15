memoryWhiteList <- c("memoryWhiteList","convertDimensionToAttribute","convertAttributeToDimension","rmTempObject","addPersistentObjects")
addPersistentObjects <- function(names){
  memoryWhiteList <<- unique(c(memoryWhiteList,names))
}
rmTempObject <- function(){
  rm(list = setdiff(ls(envir = globalenv()), memoryWhiteList),envir = globalenv())
  gc()
  ls(envir = globalenv())
}

library(plyr)
convertDimensionToAttribute<-function(data,dimensions,attributevalues,DIMENSION_NAME="ATTRIBUTE",VALUE_NAME="VALUE"){
  data$UNIQUEALL<-c(1:nrow(data))
  rownames(data)<-c(1:nrow(data))
  head(AttributeWithValue<-as.data.frame(
    as.table(
      as.matrix(
        data[,attributevalues]
      ))),5)
  colnames(AttributeWithValue)<-c("UNIQUEALL",DIMENSION_NAME,VALUE_NAME)
  #join
  system.time(result <- join(data[,append(dimensions,ncol(data))], AttributeWithValue, by = c("UNIQUEALL")))
  result$UNIQUEALL<-NULL
  head(result)
  return(result)
}
library(plyr)
#value must be integer
convertAttributeToDimensionForIntegerValue <- function(data,dimensions,attribute,value){
  dimensionsdata <- unique(data[,dimensions])
  dimensionsdata <- as.data.frame(x = dimensionsdata,stringsAsFactors = F)
  colnames(dimensionsdata) <- colnames(data)[dimensions]
  dimensionsdata$UNIQUEFROM<-c(1:nrow(dimensionsdata))
  system.time(datawithUNIQUEFROM <- join(data, dimensionsdata, by = colnames(data)[dimensions]))
  FROM_TO_VALUE_DF <- datawithUNIQUEFROM[,c(ncol(datawithUNIQUEFROM),attribute,value)]
  colnames(FROM_TO_VALUE_DF)<-c("UNIQUEFROM","TO","VALUE")
  #this why value must be integer
  FROM_TO_DF<-mdply(FROM_TO_VALUE_DF,function(UNIQUEFROM,TO,VALUE){data.frame(UNIQUEFROM=rep(UNIQUEFROM,VALUE),TO=rep(TO,VALUE))},.expand = F)
  FROM_TO_VALUEMATRIX<-as.matrix(table(FROM_TO_DF$UNIQUEFROM,FROM_TO_DF$TO))
  #join
  MatrixWithValue <- cbind(as.data.frame.matrix(FROM_TO_VALUEMATRIX),UNIQUEFROM=rownames(FROM_TO_VALUEMATRIX))
  system.time(result <- join(datawithUNIQUEFROM[,append(dimensions,ncol(datawithUNIQUEFROM))], MatrixWithValue, by = c("UNIQUEFROM")))
  result$UNIQUEFROM<-NULL
  head(result,1)
  return(result)
}

library(plyr)
#any value - character / numeric / other
convertAttributeToDimension <- function(data,dimensions,attribute,value){
  dimensionsdata <- unique(data[,dimensions])
  dimensionsdata <- as.data.frame(x = dimensionsdata,stringsAsFactors = F)
  colnames(dimensionsdata) <- colnames(data)[dimensions]
  dimensionsdata$UNIQUEFROM<-c(1:nrow(dimensionsdata))
  system.time(datawithUNIQUEFROM <- join(data, dimensionsdata, by = colnames(data)[dimensions]))
  FROM_TO_VALUE_DF <- datawithUNIQUEFROM[,c(ncol(datawithUNIQUEFROM),attribute,value)]
  colnames(FROM_TO_VALUE_DF)<-c("UNIQUEFROM","TO","VALUE")
  #apply for each 
  FROM_TO_VALUEMATRIX<-as.matrix(table(FROM_TO_DF$UNIQUEFROM,FROM_TO_DF$TO))
  m_ply(FROM_TO_VALUE_DF[1:100,],function(UNIQUEFROM,TO,VALUE,FROM_TO_VALUEMATRIX){FROM_TO_VALUEMATRIX[which(rownames(FROM_TO_VALUEMATRIX)==UNIQUEFROM),which(colnames(FROM_TO_VALUEMATRIX)==TO)] <<- VALUE},FROM_TO_VALUEMATRIX,.expand = F)
  #join
  MatrixWithValue <- cbind(as.data.frame.matrix(FROM_TO_VALUEMATRIX),UNIQUEFROM=rownames(FROM_TO_VALUEMATRIX))
  system.time(result <- join(datawithUNIQUEFROM[,append(dimensions,ncol(datawithUNIQUEFROM))], MatrixWithValue, by = c("UNIQUEFROM")))
  result$UNIQUEFROM<-NULL
  head(result)
  return(result)
}