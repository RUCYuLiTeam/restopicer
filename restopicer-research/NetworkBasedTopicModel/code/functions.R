library(tm)
library(bipartite)
library(tnet)
library(igraph)
library(slam)
library(wordcloud)
library(MASS)
library(pROC)
#library(ROCR)
library(ggplot2)
######
# plot for Matrix of bipartite
# weightType for plotRowWordCloud, plotWordCloud, plotRowDist
# for col, please use transpose = TRUE
######
plotBipartiteMatrixReport <- function(filename, bi_matrix, transpose = FALSE, showNamesInPlot = FALSE, weightType = "tfidf", plotRowWordCloud = FALSE, plotWordCloud = FALSE, plotRowComparison = FALSE, plotRowDist = FALSE, plotModules = FALSE, path = "output/"){
  # folder
  if(!file.exists(path)) dir.create(path,recursive = TRUE)
  type="o"
  if(transpose){
    bi_matrix <- as.matrix(t(bi_matrix))
    type="t"
  }
  # preprocessing for matrix
  bi_matrix_tf <- as.DocumentTermMatrix(bi_matrix,weighting = weightTf)
  bi_matrix_tfidf <- weightTfIdf(bi_matrix_tf)
  df_for_plot <- data.frame(columns = Terms(bi_matrix_tf), sumTF = col_sums(bi_matrix_tf),sumTFIDF = col_sums(bi_matrix_tfidf), stringsAsFactors = F)
  df_column <- data.frame(column_id = 1:nTerms(bi_matrix_tf), column_name = Terms(bi_matrix_tf), stringsAsFactors = F)
  df_row <- data.frame(row_id = 1:nDocs(bi_matrix_tf), column_name = Docs(bi_matrix_tf), stringsAsFactors = F)
  if(weightType=="tfidf"){
    # using when exploring importance of columns, especially for original data
    bi_data <- bi_matrix_tfidf
  }else{
    # using when exploring result of model
    bi_data <- bi_matrix_tf
    df_for_plot$sumTFIDF <- df_for_plot$sumTF
  }
  # plotRowWordCloud
  if(plotRowWordCloud){
    # different weightType of bi_data
    for(i in 1:nrow(bi_data)){
      line <- as.matrix(bi_data[i,])
      line_names <- colnames(line)[which(line!=0)]
      line_freq <- line[,which(line!=0)]
      #plot wordcloud for each line
      png(file.path(path,paste(type,filename,i,weightType,"rowwordcloud.png",sep="-")),width=600,height=600)
      par(fig = c(0,1,0.1,1),mar = c(0,0,0,0))
      pal <- brewer.pal(9,"Blues")[4:9]
      color_cluster <- pal[ceiling(6*(line/max(line)))]
      wordcloud(words=line_names,freq=line_freq,scale = c(4, 0),min.freq=1,max.words = Inf,
                random.order=F,random.color=F,rot.per=0,colors=color_cluster,ordered.colors=T,
                use.r.layout=F,fixed.asp=F)
      par(fig = c(0,1,0,0.1), mar = c(3, 2, 0, 2), new=TRUE)
      display.brewer.pal(9, "Blues")
      dev.off()
    }
  }
  if(plotWordCloud){
    # different weightType of bi_data
    #plot wordcloud
    png(file.path(path,paste(type,filename,weightType,"wordcloud.png",sep="-")),width=600,height=600)
    par(fig = c(0,1,0.1,1),mar = c(0,0,0,0))
    pal <- brewer.pal(9,"Blues")[4:9]
    color_cluster <- pal[ceiling(6*df_for_plot$sumTFIDF/max(df_for_plot$sumTFIDF))]
    wordcloud(words=df_for_plot$columns,freq=df_for_plot$sumTF,scale = c(4, 0.5),min.freq=1,max.words = Inf,
              random.order=F,random.color=F,rot.per=0,colors=color_cluster,ordered.colors=T,
              use.r.layout=F,fixed.asp=F)
    par(fig = c(0,1,0,0.1), mar = c(3, 2, 0, 2), new=TRUE)
    display.brewer.pal(9, "Blues")
    dev.off()
  }
  if(plotRowComparison){
    #no influence for different weightType of bi_data, only use tf
    data <- as.matrix(bi_matrix_tf)
    if(!showNamesInPlot) rownames(data) <- 1:nrow(data)
    #plot textplot
    png(file.path(path,paste(type,filename,"rowcomparison.png",sep="-")),width=600,height=600)
    par(fig = c(0,1,0.1,1),mar = c(0,0,0,0))
    comparison.cloud(term.matrix = t(data),title.size = 2,scale = c(4,0.5),rot.per = 0,max.words = Inf,colors = rep(brewer.pal(n = 12,name = "Paired"),ceiling(nrow(data)/12)))
    par(fig = c(0,1,0,0.1), mar = c(3, 2, 0, 2), new=TRUE)
    display.brewer.pal(12, "Paired")
    dev.off()
  }
  if(plotRowDist){
    # different weightType of bi_data
    loc <- cmdscale(dist(bi_data,method = "minkowski", p = 2))
    if(!showNamesInPlot) rownames(loc) <- 1:nrow(loc)
    #plot textplot
    png(file.path(path,paste(type,filename,weightType,"rowdist.png",sep="-")),width=600,height=600)
    par(mar=c(5,4,4,2))
    textplot(loc[,1],loc[,2],rownames(loc),cex = 2)
    dev.off()
  }
  if(plotModules){
    # different weightType of bi_data
    data <- as.matrix(bi_data)
    if(!showNamesInPlot) rownames(data) <- 1:nrow(data)
    #plot moduleWebObject
    moduleWebObject = computeModules(web = as.matrix(data),steps = 1,tolerance = 1)
    png(file.path(path,paste(type,filename,weightType,"moduleweb.png",sep="-")),width=1000,height=1000)
    par(fig = c(0,1,0,1),mar = c(0,0,0,0))
    plotModuleWeb(moduleWebObject,plotModules = TRUE, rank = FALSE, weighted = TRUE, 
                  displayAlabels = TRUE, displayBlabels = TRUE, 
                  labsize = 1, xlabel = "", ylabel = "", square.border = "white", fromDepth = 0, upToDepth = -1)
    dev.off()
  }
  if(!transpose){
    write.table(df_column,file = file.path(path,paste(filename,"column.txt",sep="-")),quote = F,sep = "\t",row.names = F,col.names = T)
    write.table(df_row,file = file.path(path,paste(filename,"row.txt",sep="-")),quote = F,sep = "\t",row.names = F,col.names = T)
  }
}
######
# run data processing for bipartite
# type : matrix and edgelist
######
runMaxCompartOfMatrix <- function(bi_data, plotCompartAnalysis = FALSE){
  bi_matrix <- as.matrix(bi_data)
  bi_compart <- compart(bi_matrix)
  compart.belong <- data.frame(row=row.names(bi_compart$cweb),compart=-apply(bi_compart$cweb,1,FUN = min))
  size.compart <-  ddply(compart.belong,.(compart),.fun = nrow)
  colnames(size.compart) <- c("compart","cnt")
  max.size.compart <- size.compart[which.max(size.compart$cnt),]$compart
  row.compart <- compart.belong[compart.belong$compart == max.size.compart,]
  bi_MaxCompart <- bi_matrix[row.names(bi_matrix) %in% row.compart$row,]
  bi_MaxCompart <- bi_MaxCompart[,colSums(bi_MaxCompart)!=0]
  #bi_MaxCompart[which(bi_MaxCompart!=0)] <- 1 # transform to binary
  bi_MaxCompart
}
runBipartiteProjecting <- function (net, method = "length", directed = F){
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
  # i is the projected part
  # w is the weight of i-p
  net2 <- net2[order(net2[, "i"], net2[, "p"]), ]
  np <- table(net2[, "p"])
  # np is the degree of p
  net2 <- merge(net2, cbind(p = as.numeric(row.names(np)),np = np))
  net1 <- merge(net2, cbind(j = net2[, "i"], p = net2[, "p"], w_j = net2[, "w"]))
  colnames(net1) <- c("p","i","w_i","np","j","w_j")
  net1 <- net1[net1[, "i"] != net1[, "j"], c("i", "w_i", "j", "w_j", "np")]
  net1 <- net1[order(net1[, "i"], net1[, "j"]), ]
  index <- !duplicated(net1[, c("i", "j")])
  w <- switch(method, binary = rep(1, sum(index)), 
              length = tapply(net1[,"np"], cumsum(index), length), 
              sumchoose = tapply(net1[,"np"], cumsum(index), function(a) sum(1/choose(a,2))))
  net1 <- cbind(net1[index, c("i", "w_i", "j", "w_j")], w = as.numeric(w))
  if(!directed){
    dup_Bool <- net1$i>net1$j
    tmp <- net1[dup_Bool,]$i
    net1[dup_Bool,]$i <- net1[dup_Bool,]$j
    net1[dup_Bool,]$j <- tmp
    net1 <- net1[order(net1[, "i"], net1[, "j"]), ]
    index <- !duplicated(net1[, c("i", "j")])
    w <- switch(method, binary = rep(1, sum(index)),
                length = tapply(net1[,"w"], cumsum(index), unique),
                sumchoose = tapply(net1[,"w"], cumsum(index), unique))
    net1 <- cbind(net1[index, c("i", "w_i", "j", "w_j")], w = as.numeric(w))
  }
  return(net1)
}
######
# plot network for bipartite and co-term
# type : matrix and edgelist
######
plotTopicNetworkReport <- function(filename,graph,community_member_list=NULL,showNamesInPlot = TRUE,plotCommunity=FALSE,plotOverallTopics=FALSE,path = "output/"){
  # folder
  if(!file.exists(path)) dir.create(path,recursive = TRUE)
  if(showNamesInPlot){
    label <- NULL
  }else{
    label <- NA
  }
  if(plotCommunity){
    # plot for each community
    for(i in 1:length(community_member_list)){
      g <- delete.vertices(graph,names(V(graph))[!names(V(graph)) %in% community_member_list[[i]]])
      png(file.path(path,paste(filename,i,"topicnetwork.png",sep="-")),width=600,height=600,pointsize=6)
      par(mar = c(5, 4, 4, 2))
      set.seed(13)
      plot(g,layout=layout.auto,vertex.label=label,
           vertex.color="steelblue",vertex.size = 2,vertex.label.color="brown",
           vertex.label.cex=2,vertex.label.font=2,vertex.label.dist=0,
           edge.color="darkgrey",edge.label.cex=1,edge.width=1.8)
      dev.off()
    }
  }
  if(plotOverallTopics){
    png(file.path(path,paste(filename,"network.png",sep="-")),width=1000,height=1000,pointsize=2)
    par(mar = c(3, 2, 0, 2))
    set.seed(13)
    comm_color <- rainbow(length(community_member_list), alpha = 0.3)
    plot(graph,layout=layout.auto,vertex.label=label,mark.shape=1,
         mark.groups=community_member_list,mark.col=comm_color,mark.expand=5,
         vertex.color="steelblue",vertex.size = 2,vertex.label.color="brown",
         vertex.label.cex=2,vertex.label.font=2,vertex.label.dist=0,
         edge.color="darkgrey",edge.label.cex=1,edge.width=1.8)
    dev.off()
  }
}
######
# Community is the topic
# return an topic-term or topic-edge bipartite matrix
# community-membership or called Topic-Member bipartite matrix
######
getTopicMemberBipartiteMatrix <- function(community_member_list, weight = "binary"){
  member <- unlist(community_member_list,use.names = F)
  community <- rep(1:length(community_member_list),times = unlist(lapply(community_member_list,FUN = length)))
  bi_matrix <- table(community,member)
  if(weight != "binary"){
    
  }
  as.matrix(bi_matrix)
}
getDocTopicBipartiteMatrix <- function(doc_member,topic_member,method = "Moore-Penrose", scaling = FALSE){
  calGeneralizedInverseMatrix <- function(){
    doc_member %*% ginv(topic_member)
  }
  calTransposMatrix <- function(){
    doc_member %*% t(topic_member)
  }
  calSimilarity.cos <- function(){
    t(apply(doc_member,1,FUN = function(doc){doc/sqrt(sum(doc^2))})) %*% t(apply(topic_member,2,FUN = function(topic){topic/sqrt(sum(topic^2))}))
  }
  M <- switch(method,
         "Moore-Penrose" = calGeneralizedInverseMatrix(),
         "Transpose" = calTransposMatrix(),
         "similarity.cos" = calSimilarity.cos())
  if(scaling){
    M <- t(scale(t(M),center = F,scale = T)) 
  }
  M
}
######
# Community Detection In R
# igraph
# http://igraph.wikidot.com/community-detection-in-r
######
# g <- sample_gnp(100, 0.3)
# k <- 6
clique.community <- function(graph, k, threshold = 1) {
  clq <- cliques(graph, min=k, max=k)
  edges <- c()
  for (i in seq_along(clq)) {
    for (j in (i+1):length(clq)) {
      # (K-threshold)/(K+threshold)
      if ( length(unique(c(clq[[i]], clq[[j]]))) <= k+threshold ) {
        edges <- c(edges, c(i,j)-1)
      }
    }
  }
  clq.graph <- simplify(graph(edges))
  V(clq.graph)$name <- seq_len(vcount(clq.graph))
  comps <- decompose.graph(clq.graph)
  
  lapply(comps, function(x) {
    unique(unlist(clq[ V(x)$name ]))
  })
}
largeScaleCommunity <- function(g,mode="all"){
  V(g)$group <- as.character(V(g))
  thisOrder <- sample(vcount(g),vcount(g))-1
  t <- 0
  done <- FALSE
  while(!done){
    t <- t+1
    cat("\rtick:",t)
    done <- TRUE ## change to FALSE whenever a node changes groups              
    for(i in thisOrder){
      ## get the neighbor group frequencies:                                    
      groupFreq <- table(V(g)[neighbors(g,i,mode=mode)]$group)
      ## pick one of the most frequent:                                         
      newGroup <- sample(names(groupFreq) [groupFreq==max(groupFreq)],1)
      if(done){done <- newGroup==V(g)[i]$group}
      V(g)[i]$group <- newGroup
    }
  }
  ## now fix any distinct groups with same labels:                              
  for(i in unique(V(g)$group)){
    ## only bother for connected groups                                         
    if(!is.connected(subgraph(g,V(g)[group==i]))){
      theseNodes <- V(g)[group==i]
      theseClusters <- clusters(subgraph(g,theseNodes))
      ## iterate through the clusters and append their names                    
      for(j in unique(theseClusters$membership)){
        V(g)[theseNodes[theseClusters$membership==j]]$group <- paste(i,j,sep=".")
      }
    }
  }
  return(g)
}
######
# Community Detection Result comparison
# Testing the significance of a community
# for different perspect
######
doc.tagging.test <- function(taggingtest_data,filename,path = "output/", LeaveOneOut = FALSE){
  # folder
  if(!file.exists(path)) dir.create(path,recursive = TRUE)
  # start
  taggingtype <- unique(taggingtest_data[,dim(taggingtest_data)[2]])
  taggingtest_result <- data.frame()
  for(type in taggingtype){
    data <- taggingtest_data[,2:dim(taggingtest_data)[2]]
    data[,dim(data)[2]] <- (taggingtest_data[,dim(taggingtest_data)[2]]==type)
    colnames(data)[dim(data)[2]] <- "binary_class"
    result <- data.frame()
    if(LeaveOneOut){
      for(i in 1:dim(data)[1]){
        train.data <- data[-i,]
        test.data <- data[i,]
        fit <- glm(binary_class ~.,family=binomial(link='logit'),data=train.data)
        test.result <- predict.glm(fit,test.data[,1:(dim(data)[2]-1)],type = "response")
        result0 <- data.frame(real.y=test.data$binary_class,model.y=(test.result>=0.5),fitted.values=test.result)
        result <- rbind(result,result0)
      }
    }else{
      fit <- glm(binary_class ~.,family=binomial(link='logit'),data=data)
      summary(fit)
      R2_Cox.Snell <- 1-exp((fit$deviance-fit$null.deviance)/nrow(fit$data))#计算Cox-Snell拟合优度
      R2_Nagelkerke <- R2_Cox.Snell/(1-exp((-fit$null.deviance)/nrow(fit$data)))#计算Nagelkerke拟合优度，我们在最后输出这个拟合优度值
      result <- data.frame(real.y=(fit$y==1),model.y=(fit$fitted.values>=0.5),fitted.values=fit$fitted.values)
    }
    #caret::confusionMatrix
    result.table <- table(factor(result$model.y,levels=c("TRUE","FALSE")),factor(result$real.y,levels=c("TRUE","FALSE")))
    #result.prediction <- prediction(result$fitted.values,result$model.y)
    # for positive class
    precision <- result.table["TRUE","TRUE"]/sum(result.table["TRUE",])
    sensitivity <- recall <- result.table["TRUE","TRUE"]/sum(result.table[,"TRUE"])
    miss_rate <- 1 - recall
    specificity <- result.table["FALSE","FALSE"]/sum(result.table[,"FALSE"])
    distribution <- prevalence <- sum(result.table[,"TRUE"])/sum(result.table)
    PPV <- (sensitivity * prevalence)/((sensitivity*prevalence) + ((1-specificity)*(1-prevalence)))
    NPV <- (specificity * (1-prevalence))/(((1-sensitivity)*prevalence) + ((specificity)*(1-prevalence)))
    detection_prevalence <- sum(result.table["TRUE",])/sum(result.table)
    detection_rate <- detection_prevalence * precision
    #detection_rate <- prevalence * sensitivity
    f_measure <- 2*precision*recall/(precision+recall)
    # for over all
    accuracy <- distribution*recall+(1-distribution)*specificity
    error_rate <- 1 - accuracy
    balanced_accuracy <- (sensitivity+specificity)/2
    Mcnemar_Test.PValue <- mcnemar.test(result.table)$p.value
    no_information_rate <- max(distribution,1-distribution)
    # roc and auc
    if(length(unique(result$fitted.values))>5){
      ROC_plot <- roc(result$real.y, result$fitted.values,smooth=T,percent=T)
    }else{
      ROC_plot <- roc(result$real.y, result$fitted.values,percent=T)
    }
    # plot ROC
    png(file.path(path,paste(filename,type,"ROC.png",sep="-")),width=500,height=500)
    par(mar=c(5,4,4,2))
    plot(ROC_plot,max.auc.polygon=T,auc.polygon=T,grid=T,show.thres=T,print.auc=T,main=type,cex.main=1)
    dev.off()
    #auc <- mean(sample(pos.decision,1000,replace=T) > sample(neg.decision,1000,replace=T))
    AUC <- ROC_plot$auc
    result.measure <- data.frame(filename,tagging.type=type,
                                 accuracy,error_rate,balanced_accuracy,
                                 Mcnemar_Test.PValue,AUC,no_information_rate,
                                 sensitivity,recall,miss_rate,specificity,
                                 distribution,prevalence,detection_prevalence,PPV,NPV,
                                 precision,detection_rate,f_measure)
    taggingtest_result <- rbind(taggingtest_result,result.measure)  
  }
  write.table(taggingtest_result,file = file.path(path,paste(filename,"taggingtest.txt",sep="-")),quote = F,sep = "\t",row.names = F,col.names = T)
}
community.significance.test <- function(graph, vs, ...) {
  if (is.directed(graph)) stop("This method requires an undirected graph")
  subgraph <- induced.subgraph(graph, vs)
  in.degrees <- degree(subgraph)
  out.degrees <- degree(graph, vs) - in.degrees
  wilcox.test(in.degrees, out.degrees, ...)
}
#####
# the last part
#####
source("code/utilities.R")