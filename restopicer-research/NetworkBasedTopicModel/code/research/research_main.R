rm(list = ls(envir = globalenv()))
options(encoding = "UTF-8")
#####
# experiment result analysis BEGIN
#####
source("code/methods.R")
# save(result_linkcomm,result_cnm,result_infomap,result_lda,
#     result_linkcomm_0.1_percolation,
#     result_linkcomm_0.15_percolation,
#     result_linkcomm_0.2_percolation,
#     file = "rdata/research_2011_2013/myanalysis.RData")
# community analysis
#load("rdata/research_2011_2013/myanalysis.RData")
LinkComm <- c(1,1.732,1.378,1.082)
InfoMap <- c(1,1,0.623,0.854)
CNM <- c(1,1,0.652,0.865)
linkcomm_0.1_percolation <- c(1,1.446,1.335,0.976)
linkcomm_0.15_percolation <- c(1,1.536,1.349,1.023)
linkcomm_0.2_percolation <- c(1,1.608,1.353,1.026)
plotCompositePerformance(filename="six",path = "output/",cols=c("red","yellow","green","blue"),LinkComm,`连边渗透(0.2)`=linkcomm_0.2_percolation,`连边渗透(0.15)`=linkcomm_0.15_percolation,`连边渗透(0.1)`=linkcomm_0.1_percolation,CNM,InfoMap)
#######
#lr model analysis
#######
rm(list = ls(envir = globalenv()))
source("code/methods.R")
load(file = "rdata/research_2011_2013.RData")
load("rdata/research_2011_2013/myanalysis.RData")
result_all <- list(result_lda,result_infomap,result_cnm,result_linkcomm,result_linkcomm_0.15_percolation)
result_fin <- lapply(result_all,function(res,papers_tags_df){
  doc_topic <- res$doc_topic
  taggingtest_doc_topic <- cbind(item_ut=rownames(doc_topic),as.data.frame(doc_topic))
  taggingtest_doc_sc <- unique(papers_tags_df[,c("item_ut","subject_category")])
  taggingtest_data <- merge(taggingtest_doc_topic, taggingtest_doc_sc)
},papers_tags_df=researchPapersSubjectCategory)
#start plot
plotcolor <- c("brown","green","red","yellow","blue")
plotlegend <- c("LDA", "InfoMap","CNM","LinkComm","连边渗透(0.15)")
plot_id <- c(1,4,5)
path="output/"
taggingtype <- unique(result_fin[[1]][,dim(result_fin[[1]])[2]])
for(type in taggingtype){
  if(file.exists(paste(type,"leaveoneout.RData",sep = "-"))){
    load(paste(type,"leaveoneout.RData",sep = "-"))
  }else{
    result_list <- list()
    for(i in 1:5){
      taggingtest_data <- result_fin[[i]]
      data <- taggingtest_data
      data[,dim(data)[2]] <- (taggingtest_data[,dim(taggingtest_data)[2]]==type)
      data <- ddply(data,1,.fun = function(doc){
        doc[,dim(doc)[2]] <- any(doc[,dim(doc)[2]])
        unique(doc)
      })
      data <- data[,-1]
      colnames(data)[dim(data)[2]] <- "binary_class"
      result <- data.frame()
      for(i in 1:dim(data)[1]){
        train.data <- data[-i,]
        test.data <- data[i,]
        fit <- glm(binary_class ~.,family=binomial(link='logit'),data=train.data)
        test.result <- predict.glm(fit,test.data[,1:(dim(data)[2]-1)],type = "response")
        result0 <- data.frame(real.y=test.data$binary_class,model.y=(test.result>=0.5),fitted.values=test.result)
        result <- rbind(result,result0)
      }
      result_list <- c(result_list,list(result))
    }
    save(result_list,file = paste(type,"leaveoneout.RData",sep = "-"))
  }
  # plot ROC
  png(file.path(path,paste(type,"ROC.png",sep="-")),width=500,height=500)
  par(mar=c(5,4,4,2))
  for(i in plot_id){
    result <- result_list[[i]]
    ROC_plot <- roc(result$real.y, result$fitted.values,percent=T)
    if(i==1){
      plot(ROC_plot,max.auc.polygon=T,auc.polygon=T,grid=T,show.thres=T,print.auc=F,main=type,cex.main=1, col=plotcolor[i])
      legend("bottomright", legend=plotlegend[plot_id],col=plotcolor[plot_id], lwd=2)
    }else{
      plot(ROC_plot,max.auc.polygon=F,auc.polygon=F,grid=F,show.thres=F,print.auc=F,main=type,cex.main=1,add=TRUE, col=plotcolor[i])
    }
  }
  dev.off()
  # plot PRC
  png(file.path(path,paste(type,"PRC.png",sep="-")),width=500,height=500)
  par(mar=c(5,4,4,2))
  for(i in plot_id){
    result <- result_list[[i]]
    ROC_plot <- roc(result$real.y, result$fitted.values,percent=T)
    PRC_plot <- ROC_plot
    PRC_prec_rec <- performance(prediction(predictions = result$fitted.values,labels = result$real.y),"prec","rec")
    PRC_plot$recall <- na.fill(PRC_prec_rec@x.values[[1]] * 100,fill = 0)
    PRC_plot$specificities <- PRC_plot$recall
    PRC_plot$precision <- na.fill(PRC_prec_rec@y.values[[1]] * 100,fill = 1)
    PRC_plot$sensitivities <- PRC_plot$precision
    if(i==1){
      plot(PRC_plot,main=type,cex.main=1,max.auc.polygon=T,grid=T, col=plotcolor[i],
           asp=1,mar=c(4, 4, 2, 2)+.1,mgp=c(2.5, 1, 0),
           lty=par("lty"),lwd=2,type="l",
           xlab="Recall (%)",ylab="Precision (%)",xlim=c(0,100),ylim=c(0,100))
      lines(x = c(0,100),y = c(0,100))
      legend("topright", legend=plotlegend[plot_id],col=plotcolor[plot_id], lwd=2)
    }else{
      plot(PRC_plot,main=type,cex.main=1,max.auc.polygon=F,grid=F,add=TRUE, col=plotcolor[i],
           asp=1,mar=c(4, 4, 2, 2)+.1,mgp=c(2.5, 1, 0),
           lty=par("lty"),lwd=2,type="l",
           xlab="Recall (%)",ylab="Precision (%)",xlim=c(0,100),ylim=c(0,100))
    }
  }
  dev.off()
}
#####
# experiment END
#####