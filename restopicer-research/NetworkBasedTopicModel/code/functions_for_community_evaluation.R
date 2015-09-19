######
# community evaluation functions and plot report
# on community_member test,topic_member test and doc_topic test
######
# comunity.coverage<-calcommunity.coverage(topic_term)
# [0,1], the larger the better
calcommunity.coverage<- function(comm_member){
  # cal non-trival comm_member matrix
  nontrival_comm<-comm_member[rowSums(comm_member)>2,]
  nontrival_comm_nodes<-nontrival_comm[,colSums(nontrival_comm)>0]
  # cal community coverage
  community.coverage<-ncol(nontrival_comm_nodes)/ncol(comm_member)
  community.coverage
}
#[1,Inf)
caloverlap.coverage<-function(comm_member){
  # cal non-trival comm_member matrix
  nontrival_comm<-comm_member[rowSums(comm_member)>2,]
  nontrival_comm_nodes<-nontrival_comm[,colSums(nontrival_comm)>0]
  mean(colSums(nontrival_comm_nodes))  
}
#the larger the denser
calcommunity.quality<-function(comm_member,coterm_g){
  coterm_matrix <- get.adjacency(coterm_g,attr = "weight",type = "upper")
  # understand 1st
#   calaverage.similarityofmatrix<-function(v){
#     used_nodes <- rownames(coterm_matrix) %in% names(v[v!=0])
#     m <- coterm_matrix[used_nodes,used_nodes]
#     sum(m)/sum(m>0)
#   }
#   enrichment <- apply(comm_member,1,calaverage.similarityofmatrix)/(sum(coterm_matrix)/sum(coterm_matrix>0))
#   mean(enrichment)
  # undestand 2nd
  calsum.similarityofmatrix<-function(v){
    used_nodes <- rownames(coterm_matrix) %in% names(v[v!=0])
    m <- coterm_matrix[used_nodes,used_nodes]
    sum(m)
  }
  enrichment <- sum(apply(comm_member,1,calsum.similarityofmatrix))/sum(coterm_matrix)
  enrichment
}
# df_bi_data <- unique(demoPapersKeywords)[,c("item_ut","author_keyword")]
# df_bi_data$author_keyword <- tolower(df_bi_data$author_keyword)
# df_bi_data <- unique(df_bi_data)
# df_doc_tag <- unique(demoPapersSubjectCategory[,c("item_ut","subject_category")])
# member_tag_df <- merge(df_bi_data,df_doc_tag)[,2:3]
caloverlap.quality<- function(community_member_list,member_tag_df){
  colnames(member_tag_df) <- c("member","tag")
  member_community_df <- ldply(community_member_list,.fun = function(member){
    data.frame(member)
  })
  colnames(member_community_df) <- c("community","member")
  member_community_tag_df <- merge(member_community_df,member_tag_df)
  cross_matrix <- table(member_community_tag_df$community,member_community_tag_df$tag)
  # cal information gain by weighted entropy
  # H_tag <- entropy.plugin(colSums(cross_matrix),unit = "log2")
  # H_tag_when_comm <- (rowSums(cross_matrix)/sum(rowSums(cross_matrix))) %*% apply(cross_matrix,1,entropy.plugin,unit = "log2")
  # IG <- H_tag - H_tag_when_comm
  # cal information gain by weighted mutual information
  entropy::mi.plugin(cross_matrix,unit = "log2")
}
caloverlap.number.quality<- function(community_member_list,member_tag_df){
  colnames(member_tag_df) <- c("member","tag")
  member_community_df <- ldply(community_member_list,.fun = function(member){
    data.frame(member)
  })
  colnames(member_community_df) <- c("community","member")
  v.model <- unlist(lapply(split(member_community_df,member_community_df$member),FUN = nrow))
  v.model <- v.model[order(names(v.model),decreasing = F)]
  v.real <- unlist(lapply(split(member_tag_df,member_tag_df$member),FUN = nrow))
  v.real <- v.real[order(names(v.real),decreasing = F)]
  # cal the cross entropy
  entropy::KL.plugin(v.model,v.real,unit = "log2")
}
# Testing the significance of a community
community.significance.test <- function(graph, vs, ...) {
  if (is.directed(graph)) stop("This method requires an undirected graph")
  subgraph <- induced.subgraph(graph, vs)
  in.degrees <- degree(subgraph)
  out.degrees <- degree(graph, vs) - in.degrees
  wilcox.test(in.degrees, out.degrees, ...)
}
# taggingtest_data is a df as doc-topics-tag
doc.tagging.test <- function(taggingtest_data,filename,path = "output/", LeaveOneOut = FALSE){
  # folder
  if(!file.exists(path)) dir.create(path,recursive = TRUE)
  # start
  taggingtype <- unique(taggingtest_data[,dim(taggingtest_data)[2]])
  taggingtest_result <- data.frame()
  for(type in taggingtype){
    data <- taggingtest_data
    data[,dim(data)[2]] <- (taggingtest_data[,dim(taggingtest_data)[2]]==type)
    data <- ddply(data,1,.fun = function(doc){
      doc[,dim(doc)[2]] <- any(doc[,dim(doc)[2]])
      unique(doc)
    })
    data <- data[,-1]
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
    # prc : precision/recall curve
    PRC_plot <- ROC_plot
    PRC_precision <- performance(prediction(predictions = result$fitted.values,labels = result$real.y),"prec")
    #specificities
    PRC_plot$specificities <- PRC_plot$recall <- ROC_plot$sensitivities
    #sensitivities
    PRC_plot$sensitivities <- PRC_plot$precision <- na.fill(PRC_precision@y.values[[1]] * 100,fill = 1)
    png(file.path(path,paste(filename,type,"PRC.png",sep="-")),width=500,height=500)
    par(mar=c(5,4,4,2))
    plot(PRC_plot,main=type,cex.main=1,max.auc.polygon=T,grid=T,
         asp=1,mar=c(4, 4, 2, 2)+.1,mgp=c(2.5, 1, 0),
         col=par("col"),lty=par("lty"),lwd=2,type="l",
         xlab="Recall (%)",ylab="Precision (%)",xlim=c(0,100),ylim=c(0,100))
    dev.off()
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