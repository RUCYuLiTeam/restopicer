######
# for general matrix plotreport or network drawing
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
      wordcloud(words=line_names,freq=line_freq,scale = c(4, 0),min.freq=1,max.words = 1000,
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
    wordcloud(words=df_for_plot$columns,freq=df_for_plot$sumTF,scale = c(4, 0.5),min.freq=1,max.words = 1000,
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
    comparison.cloud(term.matrix = t(data),title.size = 2,scale = c(4,0.5),rot.per = 0,max.words = 1000,colors = rep(brewer.pal(n = 12,name = "Paired"),ceiling(nrow(data)/12)))
    par(fig = c(0,1,0,0.1), mar = c(3, 2, 0, 2), new=TRUE)
    display.brewer.pal(12, "Paired")
    dev.off()
  }
  #if(nrow(bi_data)>100)plotRowDist <- FALSE
  if(plotRowDist){
    # different weightType of bi_data
    loc <- cmdscale(dist(bi_data,method = "minkowski", p = 1))
    if(!showNamesInPlot) rownames(loc) <- 1:nrow(loc)
    #plot textplot
    png(file.path(path,paste(type,filename,weightType,"rowdist.png",sep="-")),width=600,height=600)
    par(mar=c(5,4,4,2))
    wordcloud::textplot(x = loc[,1],y = loc[,2],words = rownames(loc),cex = 2)
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
