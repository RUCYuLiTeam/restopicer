library(tm)
library(bipartite)
library(dplyr)
library(slam)
library(wordcloud)
######
# for Document-Term Matrix
# bipartite
######
plotDocumentTermReport <- function(filename, TF_data, type = "matrix", sumTF_TopN = Inf, sumTFIDF_TopN = Inf, plotWordCloud = TRUE, plotDocComparison = FALSE, plotTermDist = FALSE, path = "output/"){
  # folder
  if(!file.exists(path)) dir.create(path,recursive = TRUE)
  # preprocessing
  if(type=="matrix"){
    TF_data <- as.DocumentTermMatrix(TF_data,weighting = weightTf)
    TFIDF_data <- weightTfIdf(TF_data)
    df_for_plot <- data.frame(Terms = Terms(TF_data), sumTF = col_sums(TF_data),sumTFIDF = col_sums(TFIDF_data), stringsAsFactors = F)
  }
  if(type=="edgelist"){
    
  }
  # get Top N
  sumTF_TopN <- ifelse(sumTF_TopN == Inf, length(df_for_plot$Terms), sumTF_TopN)
  sumTFIDF_TopN <- ifelse(sumTFIDF_TopN == Inf, length(df_for_plot$Terms), sumTFIDF_TopN)
  df_for_plot[df_for_plot$sumTF >= sort(df_for_plot$sumTF,decreasing = T)[sumTF_TopN],]
  df_for_plot[df_for_plot$sumTFIDF >= sort(df_for_plot$sumTFIDF,decreasing = T)[sumTFIDF_TopN],]
  # do scale
  df_for_plot$sumTF <- scale(df_for_plot$sumTF, center = F, scale = T)
  df_for_plot$sumTFIDF <- scale(df_for_plot$sumTFIDF, center = F, scale = T)
  loc <- cmdscale(dist(df_for_plot[,c("sumTF","sumTFIDF")],method = "canberra", p = 2))
  df_for_plot$loc_scaleSumTF <- loc[,1]
  df_for_plot$loc_scaleSumTFIDF <- loc[,2]
  if(plotWordCloud){
    #plot wordcloud
    png(file.path(path,paste(filename,"-wordcloud.png",sep="")),width=600,height=600)
    par(fig = c(0,1,0.1,1),mar = c(0,0,0,0))
    pal <- brewer.pal(9,"Blues")[4:9]
    color_cluster <- pal[ceiling(6*df_for_plot$sumTFIDF/max(df_for_plot$sumTFIDF))]
    wordcloud(words=df_for_plot$Terms,freq=df_for_plot$sumTF,scale = c(4, 0.5),min.freq=1,max.words = Inf,
              random.order=F,random.color=F,rot.per=0,colors=color_cluster,ordered.colors=T,
              use.r.layout=F,fixed.asp=F)
    par(fig = c(0,1,0,0.1), mar = c(3, 2, 0, 2), new=TRUE)
    display.brewer.pal(9, "Blues")
    dev.off()
  }
  if(plotDocComparison){
    data <- as.matrix(TFIDF_data)
    rownames(data) <- 1:nrow(data)
    #plot textplot
    png(file.path(path,paste(filename,"-doccomparison.png",sep="")),width=600,height=600)
    par(fig = c(0,1,0.1,1),mar = c(0,0,0,0))
    comparison.cloud(term.matrix = t(data),title.size = 2,scale = c(4,0.5),rot.per = 0,max.words = Inf,colors = rep(brewer.pal(n = 12,name = "Paired"),ceiling(nrow(data)/12)))
    par(fig = c(0,1,0,0.1), mar = c(3, 2, 0, 2), new=TRUE)
    display.brewer.pal(12, "Paired")
    dev.off()
  }
  if(plotTermDist){
    #plot textplot
    png(file.path(path,paste(filename,"-termdist.png",sep="")),width=600,height=600)
    par(mar=c(5,4,4,2))
    textplot(loc[,1],loc[,2],rownames(loc),cex = 1)
    dev.off()
  }
  write.table(df_for_plot,file = file.path(path,paste(filename,"-plot.txt",sep="")),quote = F,sep = "\t",row.names = F,col.names = T)
}
runMaxCompartOfBipartite <- function(bi_data, type = "matrix", plotCompartAnalysis = FALSE){
  if(type=="matrix"){
    bi_matrix <- bi_data
    bi_compart <- compart(bi_matrix)
    #bi_compart$n.compart
    size.compart <- data.frame(doc=row.names(bi_compart$cweb),compart=-apply(bi_compart$cweb,1,FUN = min)) %>% group_by(compart) %>% summarise(cnt = n())
    max.size.compart <- size.compart[which.max(size.compart$cnt),]$compart
    doc.compart <- data.frame(doc=row.names(bi_compart$cweb),compart=-apply(bi_compart$cweb,1,FUN = min)) %>% filter(compart == max.size.compart)
    bi_MaxCompart <- bi_matrix[row.names(bi_matrix) %in% doc.compart$doc,]
    #bi_MaxCompart[which(bi_MaxCompart!=0)] <- 1 # transform to binary
    bi_MaxCompart <- empty(bi_MaxCompart)
  }
  bi_MaxCompart
}
# bug fix for projecting_tm
runBipartiteProjecting <- function (net, method = "Newman", directed = F){
  if (is.null(attributes(net)$tnet)) {
    if (ncol(net) == 3) {
      net <- as.tnet(net, type = "weighted two-mode tnet")
    }
    else {
      net <- as.tnet(net, type = "binary two-mode tnet")
    }
  }
  if (attributes(net)$tnet != "binary two-mode tnet" & attributes(net)$tnet != "weighted two-mode tnet") 
    stop("Network not loaded properly")
  net2 <- net
  if (attributes(net)$tnet == "binary two-mode tnet") 
    net2 <- cbind(net2, w = 1)
  net2 <- net2[order(net2[, "i"], net2[, "p"]), ]
  np <- table(net2[, "p"])
  net2 <- merge(net2, cbind(p = as.numeric(row.names(np)),np = np))
  net1 <- merge(net2, cbind(j = net2[, "i"], p = net2[, "p"]))
  net1 <- net1[net1[, "i"] != net1[, "j"], c("i", "j", "w", "np")]
  net1 <- net1[order(net1[, "i"], net1[, "j"]), ]
  index <- !duplicated(net1[, c("i", "j")])
  w <- switch(method, binary = rep(1, sum(index)), 
              sum = tapply(net1[,"w"], cumsum(index), sum), 
              Newman = tapply(1:nrow(net1), cumsum(index), function(a) sum(net1[a, "w"]/(net1[a,"np"] - 1))))
  net1 <- cbind(net1[index, c("i", "j")], w = as.numeric(w))
  if(!directed){
    dup_Bool <- net1$i>net1$j
    tmp <- net1[dup_Bool,]$i
    net1[dup_Bool,]$i <- net1[dup_Bool,]$j
    net1[dup_Bool,]$j <- tmp
    net1 <- net1[order(net1[, "i"], net1[, "j"]), ]
    index <- !duplicated(net1[, c("i", "j")])
    w <- switch(method, binary = tapply(net1[,"w"], cumsum(index), sum),
                sum = tapply(net1[,"w"], cumsum(index), mean),
                Newman = tapply(net1[,"w"], cumsum(index), mean))
    net1 <- cbind(net1[index, c("i", "j")], w = as.numeric(w))
  }
  return(as.tnet(net1, type = "weighted one-mode tnet"))
}
######
# topics
# for topic-term matrix
######
plotTopicTermReport <- function(filename, data, plotWordCloud = TRUE, plotTopicComparison = FALSE, plotTopicDist = FALSE, path = "output/"){
  # folder
  if(!file.exists(path)) dir.create(path,recursive = TRUE)
  if(plotWordCloud){
    for(i in 1:nrow(data)){
      topic <- data[i,]
      #plot wordcloud
      png(file.path(path,paste(filename,"-topic",i,"-wordcloud.png",sep="")),width=600,height=600)
      par(fig = c(0,1,0.1,1),mar = c(0,0,0,0))
      pal <- brewer.pal(9,"Blues")[4:9]
      color_cluster <- pal[ceiling(6*(topic/max(topic)))]
      wordcloud(words=names(topic),freq=topic,scale = c(4, 0.5),min.freq=1,max.words = Inf,
                random.order=F,random.color=F,rot.per=0,colors=color_cluster,ordered.colors=T,
                use.r.layout=F,fixed.asp=F)
      par(fig = c(0,1,0,0.1), mar = c(3, 2, 0, 2), new=TRUE)
      display.brewer.pal(9, "Blues")
      dev.off()
    }
  }
  if(plotTopicComparison){
    #plot textplot
    png(file.path(path,paste(filename,"-topiccomparison.png",sep="")),width=600,height=600)
    par(fig = c(0,1,0.1,1),mar = c(0,0,0,0))
    comparison.cloud(term.matrix = t(data),title.size = 5,scale = c(4,0.5),rot.per = 0,max.words = Inf,colors = rep(brewer.pal(n = 12,name = "Paired"),ceiling(nrow(data)/12)))
    par(fig = c(0,1,0,0.1), mar = c(3, 2, 0, 2), new=TRUE)
    display.brewer.pal(12, "Paired")
    dev.off()
  }
  if(plotTopicDist){
    loc <- cmdscale(dist(data,method = "minkowski", p = 1))
    #plot textplot
    png(file.path(path,paste(filename,"-topicdist.png",sep="")),width=600,height=600)
    par(mar=c(5,4,4,2))
    textplot(loc[,1],loc[,2],rownames(loc),cex = 2)
    dev.off()
  }
  write.table(cbind(topic=rownames(data),as.data.frame(data)),file = file.path(path,paste(filename,"-topictermmatrix.txt",sep="")),quote = F,sep = "\t",row.names = F,col.names = T)
}
######
# for doc-topic matrix
######
plotDocTopicReport <- function(filename, data, path = "output/"){
  # folder
  if(!file.exists(path)) dir.create(path,recursive = TRUE)
  loc <- cmdscale(dist(data,method = "minkowski", p = 2))
  #plot textplot
  png(file.path(path,paste(filename,"-docdist.png",sep="")),width=600,height=600)
  par(mar=c(5,4,4,2))
  #textplot(loc[,1],loc[,2],rownames(loc),cex = 2)
  textplot(loc[,1],loc[,2],1:nrow(loc),cex = 2)
  dev.off()
  write.table(cbind(doc=rownames(data),as.data.frame(data)),file = file.path(path,paste(filename,"-doctopicmatrix.txt",sep="")),quote = F,sep = "\t",row.names = F,col.names = T)
}
#####
# the last part
#####
source("code/utilities.R")