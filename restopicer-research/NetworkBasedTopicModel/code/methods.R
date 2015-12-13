options(encoding = "UTF-8")
#https://en.wikipedia.org/wiki/Community_structure#Minimum-cut_method
#http://stats.stackexchange.com/questions/25007/fitting-the-parameters-of-a-stable-distribution
#####
# topic discovery
#####
# http://yuedu.baidu.com/ebook/d0b441a8ccbff121dd36839a
# http://blog.csdn.net/pirage/article/details/9467547
topicDiscovery.LDA <- function(data,datatype="abstract",
                               K=10,LDA_method="Gibbs",
                               plotPath="output/demo",plotReport=TRUE,papers_tags_df=NULL){
  # step 1:preprocessing corpus
  corpus_dtm <- switch(datatype,
              "abstract" = preprocess.abstract.corpus(data),
              "keywords" = preprocess.keywords.corpus(data))
  # step 2:runing LDA model 
  SEED <- 19910513
  corpus_topic <- switch(LDA_method,
                       "Gibbs" = LDA(corpus_dtm, k = K, method = LDA_method, control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000)),
                       "VEM" = LDA(corpus_dtm, k = K,method = LDA_method, control = list(seed = SEED)))
  # step 3:doc_topic and topic_term matrix
  topic_posterior <- posterior(corpus_topic)
  doc_topic <- topic_posterior$topics
  topic_term <- topic_posterior$terms
  # step 4:plot Report
  model <- NULL
  # filenames and foldername
  model$parameter <- paste(datatype,"LDA",K,LDA_method,sep = "_")
  #perplexity
  model$perplexity <- perplexity(corpus_topic,corpus_dtm)
  #entropy
  model$entropy <- mean(apply(doc_topic,1,function(z) -sum(z*log(z))))
  # folder
  if(!file.exists(file.path(plotPath,model$parameter))) dir.create(file.path(plotPath,model$parameter),recursive = TRUE)
  write.table(as.data.frame(model),file = file.path(plotPath,model$parameter,"modeltest.txt"),quote = F,sep = "\t",row.names = F,col.names = T)
  # doc_topic for taggingtest
  doc_topic.taggingtest(doc_topic,papers_tags_df,filename = model$parameter,path = paste(plotPath,model$parameter,sep = "/"),LeaveOneOut = T)
  if(plotReport){
    # matrix plot
    plotReport.bipartite.matrix(corpus_dtm,topic_term,doc_topic,filename = model$parameter,path = paste(plotPath,model$parameter,sep = "/"))
  }
  result <- NULL
  result$model <- model
  result$topic_term <- topic_term
  result$doc_topic <- doc_topic
  return(result)
}

topicDiscovery.infomap <- function(data,datatype="keywords",MST_Threshold=0,K=0,
                                      topic_term_weight="degree",doc_topic_method="similarity.cos",
                                      plotPath="output/demo",plotReport=TRUE,papers_tags_df=NULL){
  # step 1:preprocessing corpus and get bipartite from incidence matrix
  corpus_dtm <- switch(datatype,
                       "abstract" = preprocess.abstract.corpus(data),
                       "keywords" = preprocess.keywords.corpus(data))
  bi_graph <- graph_from_incidence_matrix(corpus_dtm)
  # step 2:projecting and network_backbone_extract
  if(MST_Threshold!=0){
    MST_Threshold
  }else{
    proj_graph <- bipartite_projection(bi_graph, types = NULL, multiplicity = TRUE,probe1 = NULL, which = "true", remove.type = TRUE)
  }
  coterm_graph <- simplify(proj_graph)
  # step 3:run infomap community
  if(file.exists("rdata/tmp/imc.RData")){
    load(file = "rdata/tmp/imc.RData")
  }else{
    imc <- infomap.community(coterm_graph)
    save(imc,file = "rdata/tmp/imc.RData")
  }
  if(K!=0){imc$membership<-cutat(imc,no=K)}
  community_member_list <- communities(imc)
  # step 4:doc_topic and topic_term matrix through community_member_list
  # generate topic-term matrix through community
  topic_term <- getTopicMemberBipartiteMatrix(community_member_list,weight = topic_term_weight,graph = coterm_graph)
  # calculate similarity to get doc-topic matrix
  doc_topic <- getDocTopicBipartiteMatrix(doc_member = corpus_dtm,topic_member = topic_term,method = doc_topic_method)
  # step 5:plot Report
  model <- NULL
  # filenames and foldername
  model$parameter <- paste(datatype,"infomap",MST_Threshold,K,topic_term_weight,doc_topic_method,sep = "_")
  #modularity
  model$modularity <- modularity(imc)
  #entropy
  model$entropy <- mean(apply(doc_topic,1,function(z) -sum(ifelse(z==0,0,z*log(z)))))
  # folder
  if(!file.exists(file.path(plotPath,model$parameter))) dir.create(file.path(plotPath,model$parameter),recursive = TRUE)
  write.table(as.data.frame(model),file = file.path(plotPath,model$parameter,"modeltest.txt"),quote = F,sep = "\t",row.names = F,col.names = T)
  # doc_topic for taggingtest
  doc_topic.taggingtest(doc_topic,papers_tags_df,filename = model$parameter,path = paste(plotPath,model$parameter,sep = "/"),LeaveOneOut = T)
  if(plotReport){
    # matrix plot
    plotReport.bipartite.matrix(corpus_dtm,topic_term,doc_topic,filename = model$parameter,path = paste(plotPath,model$parameter,sep = "/"),drawNetwork=T,coterm_graph=coterm_graph,community_member_list=community_member_list)
  }
  result <- NULL
  result$model <- model
  result$topic_term <- topic_term
  result$doc_topic <- doc_topic
  result$community_member_list <- community_member_list
  result$infomap<-comm_member.communitytest(community_member_list,bi_data_df = data,coterm_graph,papers_tags_df)
  return(result)
}

topicDiscovery.fastgreedy <- function(data,datatype="keywords",MST_Threshold=0,K=0,
                                      topic_term_weight="degree",doc_topic_method="similarity.cos",
                                      plotPath="output/demo",plotReport=TRUE,papers_tags_df=NULL){
  # step 1:preprocessing corpus and get bipartite from incidence matrix
  corpus_dtm <- switch(datatype,
                       "abstract" = preprocess.abstract.corpus(data),
                       "keywords" = preprocess.keywords.corpus(data))
  bi_graph <- graph_from_incidence_matrix(corpus_dtm)
  # step 2:projecting and network_backbone_extract
  if(MST_Threshold!=0){
    MST_Threshold
  }else{
    proj_graph <- bipartite_projection(bi_graph, types = NULL, multiplicity = TRUE,probe1 = NULL, which = "true", remove.type = TRUE)
  }
  coterm_graph <- simplify(proj_graph)
  # step 3:run fastgreedy community
  if(file.exists("rdata/tmp/fc.RData")){
    load(file = "rdata/tmp/fc.RData")
  }else{
    fc <- fastgreedy.community(coterm_graph)
    save(fc,file = "rdata/tmp/fc.RData")
  }
  if(K!=0){fc$membership<-cutat(fc,no=K)}
  community_member_list <- communities(fc)
  # step 4:doc_topic and topic_term matrix through community_member_list
  # generate topic-term matrix through community
  topic_term <- getTopicMemberBipartiteMatrix(community_member_list,weight = topic_term_weight,graph = coterm_graph)
  # calculate similarity to get doc-topic matrix
  doc_topic <- getDocTopicBipartiteMatrix(doc_member = corpus_dtm,topic_member = topic_term,method = doc_topic_method)
  # step 5:plot Report
  model <- NULL
  # filenames and foldername
  model$parameter <- paste(datatype,"fastgreedy",MST_Threshold,K,topic_term_weight,doc_topic_method,sep = "_")
  #modularity
  model$modularity <- modularity(fc)
  #entropy
  model$entropy <- mean(apply(doc_topic,1,function(z) -sum(ifelse(z==0,0,z*log(z)))))
  # folder
  if(!file.exists(file.path(plotPath,model$parameter))) dir.create(file.path(plotPath,model$parameter),recursive = TRUE)
  write.table(as.data.frame(model),file = file.path(plotPath,model$parameter,"modeltest.txt"),quote = F,sep = "\t",row.names = F,col.names = T)
  # doc_topic for taggingtest
  doc_topic.taggingtest(doc_topic,papers_tags_df,filename = model$parameter,path = paste(plotPath,model$parameter,sep = "/"),LeaveOneOut = T)
  if(plotReport){
    # matrix plot
    plotReport.bipartite.matrix(corpus_dtm,topic_term,doc_topic,filename = model$parameter,path = paste(plotPath,model$parameter,sep = "/"),drawNetwork=T,coterm_graph=coterm_graph,community_member_list=community_member_list)
  }
  result <- NULL
  result$model <- model
  result$topic_term <- topic_term
  result$doc_topic <- doc_topic
  result$community_member_list <- community_member_list
  result$fastgreedy<-comm_member.communitytest(community_member_list,bi_data_df = data,coterm_graph,papers_tags_df)
  return(result)
}

topicDiscovery.clique.percolation <- function(data,datatype="keywords",MST_Threshold=0,clique_k=30,
                                                topic_term_weight="degree",doc_topic_method="similarity.cos",
                                                plotPath="output/demo",plotReport=TRUE,papers_tags_df=NULL){
  # step 1:preprocessing corpus and get bipartite from incidence matrix
  corpus_dtm <- switch(datatype,
                       "abstract" = preprocess.abstract.corpus(data),
                       "keywords" = preprocess.keywords.corpus(data))
  bi_graph <- graph_from_incidence_matrix(corpus_dtm)
  # step 2:projecting and network_backbone_extract
  if(MST_Threshold!=0){
    MST_Threshold
  }else{
    proj_graph <- bipartite_projection(bi_graph, types = NULL, multiplicity = TRUE,probe1 = NULL, which = "true", remove.type = TRUE)
  }
  coterm_graph <- simplify(proj_graph)
  # step 3:run clique community
  # self-defined link_similarity_method for dist
  community_member_list <- clique.percolation.community(graph =coterm_graph ,k =clique_k ,threshold =1 ,cutat =NULL )
  # step 4:doc_topic and topic_term matrix through community_member_list
  # generate topic-term matrix through community
  topic_term <- getTopicMemberBipartiteMatrix(community_member_list,weight = topic_term_weight,graph = coterm_graph)
  # calculate similarity to get doc-topic matrix
  doc_topic <- getDocTopicBipartiteMatrix(doc_member = corpus_dtm,topic_member = topic_term,method = doc_topic_method)
  # step 5:plot Report
  model <- NULL
  # filenames and foldername
   model$parameter <- paste(datatype,"clique.percolation",MST_Threshold,clique_k,topic_term_weight,doc_topic_method,sep = "_")
  #entropy
  model$entropy <- mean(apply(doc_topic,1,function(z) -sum(ifelse(z==0,0,z*log(z)))))
  # folder
  if(!file.exists(file.path(plotPath,model$parameter))) dir.create(file.path(plotPath,model$parameter),recursive = TRUE)
  write.table(as.data.frame(model),file = file.path(plotPath,model$parameter,"modeltest.txt"),quote = F,sep = "\t",row.names = F,col.names = T)
  # doc_topic for taggingtest
  doc_topic.taggingtest(doc_topic,papers_tags_df,filename = model$parameter,path = paste(plotPath,model$parameter,sep = "/"),LeaveOneOut = T)
  if(plotReport){
    # matrix plot
    plotReport.bipartite.matrix(corpus_dtm,topic_term,doc_topic,filename = model$parameter,path = paste(plotPath,model$parameter,sep = "/"),drawNetwork=T,coterm_graph=coterm_graph,community_member_list=community_member_list)
  }
  result <- NULL
  result$model <- model
  result$topic_term <- topic_term
  result$doc_topic <- doc_topic
  result$community_member_list <- community_member_list
  result$clique<-comm_member.communitytest(community_member_list,bi_data_df = data,coterm_graph,papers_tags_df)
  return(result)
}

topicDiscovery.linkcomm <- function(data,datatype="keywords",MST_Threshold=0,cutat = NULL,
                                    topic_term_weight="degree",doc_topic_method="similarity.cos",
                                    plotPath="output/demo",plotReport=TRUE,papers_tags_df=NULL,link_similarity_method="original"){
  # step 1:preprocessing corpus and get bipartite from incidence matrix
  corpus_dtm <- switch(datatype,
                       "abstract" = preprocess.abstract.corpus(data),
                       "keywords" = preprocess.keywords.corpus(data))
  bi_graph <- graph_from_incidence_matrix(corpus_dtm)
  # step 2:projecting and network_backbone_extract
  if(MST_Threshold!=0){
    MST_Threshold
  }else{
    proj_graph <- bipartite_projection(bi_graph, types = NULL, multiplicity = TRUE,probe1 = NULL, which = "true", remove.type = TRUE)
  }
  coterm_graph <- simplify(proj_graph)
  # step 3:run Link community
  coterm_edgelist <- as.data.frame(cbind(get.edgelist(coterm_graph),get.edge.attribute(coterm_graph,name = "weight")),stringsAsFactors = F)
  coterm_edgelist$V3 <- as.numeric(coterm_edgelist$V3)
  # self-defined link_similarity_method for dist
  dist <- NULL
  if(link_similarity_method!="original"){
    
  }
  if(file.exists("rdata/tmp/lc.RData")){
    load(file = "rdata/tmp/lc.RData")
  }else{
    lc <- getLinkCommunities(coterm_edgelist,hcmethod="average",bipartite=F,dist = dist,plot = F)
    save(lc,file = "rdata/tmp/lc.RData")
  }
  if(!is.null(cutat)){
    lc <- newLinkCommsAt(lc, cutat = cutat)
  }
  community_member_list <- lapply(split(lc$nodeclusters$node,f = lc$nodeclusters$cluster),FUN = function(x){unlist(as.character(x))})
  # step 4:doc_topic and topic_term matrix through community_member_list
  # generate topic-term matrix through community
  topic_term <- getTopicMemberBipartiteMatrix(community_member_list,weight = topic_term_weight,graph = coterm_graph)
  # calculate similarity to get doc-topic matrix
  doc_topic <- getDocTopicBipartiteMatrix(doc_member = corpus_dtm,topic_member = topic_term,method = doc_topic_method)
  # step 5:plot Report
  model <- NULL
  # filenames and foldername
  model$parameter <- paste(datatype,"linkcomm",MST_Threshold,link_similarity_method,topic_term_weight,doc_topic_method,sep = "_")
  #partition Densities
  model$partitiondensity <- 2*sum(LinkDensities(lc)*sapply(lc$clusters,FUN = length))/lc$numbers[1]
  #entropy
  model$entropy <- mean(apply(doc_topic,1,function(z) -sum(ifelse(z==0,0,z*log(z)))))
  # folder
  if(!file.exists(file.path(plotPath,model$parameter))) dir.create(file.path(plotPath,model$parameter),recursive = TRUE)
  write.table(as.data.frame(model),file = file.path(plotPath,model$parameter,"modeltest.txt"),quote = F,sep = "\t",row.names = F,col.names = T)
  # doc_topic for taggingtest
  doc_topic.taggingtest(doc_topic = doc_topic,papers_tags_df = papers_tags_df,filename = model$parameter,path = paste(plotPath,model$parameter,sep = "/"),LeaveOneOut = T)
  if(plotReport){
    # matrix plot
    plotReport.bipartite.matrix(corpus_dtm,topic_term,doc_topic,filename = model$parameter,path = paste(plotPath,model$parameter,sep = "/"),drawNetwork=T,coterm_graph=coterm_graph,community_member_list=community_member_list)
  }
  result <- NULL
  result$model <- model
  result$topic_term <- topic_term
  result$doc_topic <- doc_topic
  result$community_member_list <- community_member_list
  result$linkcomm<-comm_member.communitytest(community_member_list,bi_data_df = data,coterm_graph,papers_tags_df)
  return(result)
}

topicDiscovery.linkcomm.percolation <- function(data,datatype="keywords",MST_Threshold=0,percolation_threshold=0.5,cutat = NULL,
                                    topic_term_weight="degree",doc_topic_method="similarity.cos",
                                    plotPath="output/demo",plotReport=TRUE,papers_tags_df=NULL,link_similarity_method="original"){
  # step 1:preprocessing corpus and get bipartite from incidence matrix
  corpus_dtm <- switch(datatype,
                       "abstract" = preprocess.abstract.corpus(data),
                       "keywords" = preprocess.keywords.corpus(data))
  bi_graph <- graph_from_incidence_matrix(corpus_dtm)
  # step 2:projecting and network_backbone_extract
  if(MST_Threshold!=0){
    MST_Threshold
  }else{
    proj_graph <- bipartite_projection(bi_graph, types = NULL, multiplicity = TRUE,probe1 = NULL, which = "true", remove.type = TRUE)
  }
  coterm_graph <- simplify(proj_graph)
  # step 3:run Link community
  coterm_edgelist <- as.data.frame(cbind(get.edgelist(coterm_graph),get.edge.attribute(coterm_graph,name = "weight")),stringsAsFactors = F)
  coterm_edgelist$V3 <- as.numeric(coterm_edgelist$V3)
  # self-defined link_similarity_method for dist
  dist <- NULL
  if(link_similarity_method!="original"){
    
  }
  community_member_list <- linkcomm.percolation.community(g_edgelist = coterm_edgelist,bipartite = F,dist = dist,threshold = percolation_threshold,cutat = cutat)
  # step 4:doc_topic and topic_term matrix through community_member_list
  # generate topic-term matrix through community
  topic_term <- getTopicMemberBipartiteMatrix(community_member_list,weight = topic_term_weight,graph = coterm_graph)
  # calculate similarity to get doc-topic matrix
  doc_topic <- getDocTopicBipartiteMatrix(doc_member = corpus_dtm,topic_member = topic_term,method = doc_topic_method)
  # step 5:plot Report
  model <- NULL
  # filenames and foldername
  if(is.null(cutat)){
    model$parameter <- paste(datatype,"linkcomm.percolation",MST_Threshold,percolation_threshold,link_similarity_method,topic_term_weight,doc_topic_method,sep = "_")
  }else{
    model$parameter <- paste(datatype,"linkcomm.percolation",MST_Threshold,percolation_threshold,cutat,link_similarity_method,topic_term_weight,doc_topic_method,sep = "_")
  }
  #entropy
  model$entropy <- mean(apply(doc_topic,1,function(z) -sum(ifelse(z==0,0,z*log(z)))))
  # folder
  if(!file.exists(file.path(plotPath,model$parameter))) dir.create(file.path(plotPath,model$parameter),recursive = TRUE)
  write.table(as.data.frame(model),file = file.path(plotPath,model$parameter,"modeltest.txt"),quote = F,sep = "\t",row.names = F,col.names = T)
  # doc_topic for taggingtest
  doc_topic.taggingtest(doc_topic,papers_tags_df,filename = model$parameter,path = paste(plotPath,model$parameter,sep = "/"),LeaveOneOut = T)
  if(plotReport){
    # matrix plot
    plotReport.bipartite.matrix(corpus_dtm,topic_term,doc_topic,filename = model$parameter,path = paste(plotPath,model$parameter,sep = "/"),drawNetwork=T,coterm_graph=coterm_graph,community_member_list=community_member_list)
  }
  result <- NULL
  result$model <- model
  result$topic_term <- topic_term
  result$doc_topic <- doc_topic
  result$community_member_list <- community_member_list
  result$linkcomm<-comm_member.communitytest(community_member_list,bi_data_df = data,coterm_graph,papers_tags_df)
  return(result)
}

topicDiscovery.linkcomm.percolation.edge <- function(data,datatype="keywords",MST_Threshold=0,percolation_threshold=0.5,cutat_th = NULL,
                                                topic_term_weight="evcent",doc_topic_method="similarity.cos",
                                                plotPath="output/demo",plotReport=TRUE,papers_tags_df=NULL,link_similarity_method="original"){
  # step 1:preprocessing corpus and get bipartite from incidence matrix
  corpus_dtm <- switch(datatype,
                       "abstract" = preprocess.abstract.corpus(data),
                       "keywords" = preprocess.keywords.corpus(data))
  bi_graph <- graph_from_incidence_matrix(corpus_dtm)
  # step 2:projecting and network_backbone_extract
  if(MST_Threshold!=0){
    MST_Threshold
  }else{
    proj_graph <- bipartite_projection(bi_graph, types = NULL, multiplicity = TRUE,probe1 = NULL, which = "true", remove.type = TRUE)
  }
  coterm_graph <- simplify(proj_graph)
  # step 3:run Link community
  coterm_edgelist <- as.data.frame(cbind(get.edgelist(coterm_graph),get.edge.attribute(coterm_graph,name = "weight")),stringsAsFactors = F)
  coterm_edgelist$V3 <- as.numeric(coterm_edgelist$V3)
  # self-defined link_similarity_method for dist
  dist <- NULL
  if(link_similarity_method!="original"){
    
  }
  #coterm_edgelist as edge id
  community_member_list <- linkcomm.percolation(g_edgelist = coterm_edgelist,bipartite = F,dist = dist,threshold = percolation_threshold,cutat_th = cutat_th)
  # step 4:doc_topic and topic_term matrix through community_member_list
  # generate topic-term matrix through community
  topic_term <- getTopicMemberBipartiteMatrix(community_member_list,weight = topic_term_weight,graph = coterm_graph,memberType="edge")
  # calculate similarity to get doc-topic matrix
  doc_topic <- getDocTopicBipartiteMatrix(doc_member = corpus_dtm,topic_member = topic_term,method = doc_topic_method)
  # step 5:plot Report
  model <- NULL
  # filenames and foldername
  if(is.null(cutat_th)){
    model$parameter <- paste(datatype,"linkcomm.percolation",MST_Threshold,percolation_threshold,link_similarity_method,topic_term_weight,doc_topic_method,sep = "_")
  }else{
    model$parameter <- paste(datatype,"linkcomm.percolation",MST_Threshold,percolation_threshold,cutat_th,link_similarity_method,topic_term_weight,doc_topic_method,sep = "_")
  }
  #entropy
  model$entropy <- mean(apply(doc_topic,1,function(z) -sum(ifelse(z==0,0,z*log(z)))))
  # folder
  if(!file.exists(file.path(plotPath,model$parameter))) dir.create(file.path(plotPath,model$parameter),recursive = TRUE)
  write.table(as.data.frame(model),file = file.path(plotPath,model$parameter,"modeltest.txt"),quote = F,sep = "\t",row.names = F,col.names = T)
  # doc_topic for taggingtest
  # doc_topic.taggingtest(doc_topic,papers_tags_df,filename = model$parameter,path = paste(plotPath,model$parameter,sep = "/"),LeaveOneOut = T)
  if(plotReport){
    # matrix plot
    plotReport.bipartite.matrix(corpus_dtm,topic_term,doc_topic,filename = model$parameter,path = paste(plotPath,model$parameter,sep = "/"),drawNetwork=T,coterm_graph=coterm_graph,community_member_list=community_member_list)
  }
  result <- NULL
  result$model <- model
  result$topic_term <- topic_term
  result$doc_topic <- doc_topic
  result$community_member_list <- community_member_list
  #result$linkcomm<-comm_member.communitytest(community_member_list,bi_data_df = data,coterm_graph,papers_tags_df)
  result$coterm_graph <- coterm_graph
  return(result)
}

topicDiscovery.linkcomm.bipartite <- function(data,datatype="keywords",weight="degree",
                                    plotPath="output/demo",plotReport=TRUE,papers_tags_df=NULL,link_similarity_method="original"){
  # step 1:preprocessing corpus and get bipartite from incidence matrix
  corpus_dtm <- switch(datatype,
                       "abstract" = preprocess.abstract.corpus(data),
                       "keywords" = preprocess.keywords.corpus(data))
  bi_graph <- graph_from_incidence_matrix(corpus_dtm)
  # step 2:run Link community on bipartite
  bi_edgelist <- as_edgelist(bi_graph)
  # self-defined link_similarity_method for dist
  dist <- NULL
  if(link_similarity_method!="original"){
    
  }
  lc <- getLinkCommunities(bi_edgelist,hcmethod="average",bipartite=T,dist = dist,plot = F)
  edge_community_df <- lc$edges
  # step 3:doc_topic and topic_term matrix through edge_community_df
  result <- getCommunityMemberBipartiteMatrix(edge_community_df,weight = weight)
  topic_term <- result$topic_term
  doc_topic <- result$doc_topic
  # step 4:plot Report
  model <- NULL
  # filenames and foldername
  model$parameter <- paste(datatype,"linkcomm_bipartite",link_similarity_method,weight,sep = "_")
  #partition Densities
  model$partitiondensity <- 2*sum(LinkDensities(lc)*sapply(lc$clusters,FUN = length))/lc$numbers[1]
  #entropy
  model$entropy <- mean(apply(doc_topic,1,function(z) -sum(ifelse(z==0,0,z*log(z)))))
  # folder
  if(!file.exists(file.path(plotPath,model$parameter))) dir.create(file.path(plotPath,model$parameter),recursive = TRUE)
  write.table(as.data.frame(model),file = file.path(plotPath,model$parameter,"modeltest.txt"),quote = F,sep = "\t",row.names = F,col.names = T)
  # doc_topic for taggingtest
  doc_topic.taggingtest(doc_topic,papers_tags_df,filename = model$parameter,path = paste(plotPath,model$parameter,sep = "/"),LeaveOneOut = T)
  if(plotReport){
    # matrix plot
    plotReport.bipartite.matrix(corpus_dtm,topic_term,doc_topic,filename = model$parameter,path = paste(plotPath,model$parameter,sep = "/"),drawNetwork=F,coterm_graph=NULL,community_member_list=NULL,bipartite=T,edge_community_df=edge_community_df)
  }
  result <- NULL
  result$model <- model
  result$topic_term <- topic_term
  result$doc_topic <- doc_topic
  result$edge_community_df <- edge_community_df
  return(result)
}
#####
# preprocessing method
#####
preprocess.abstract.corpus <- function(papers_df){
  data <- unique(papers_df)
  corpus <- VCorpus(VectorSource(data$abstract))
  l_ply(.data = 1:length(corpus),.fun = function(i){
    meta(corpus[[i]],tag = "id") <<- data$item_ut[i]
    meta(corpus[[i]],tag = "title") <<- data$article_title[i]
    meta(corpus[[i]],tag = "cited_count") <<- data$cited_count[i]
    meta(corpus[[i]],tag = "document_type") <<- data$document_type[i]
    meta(corpus[[i]],tag = "publisher") <<- data$full_source_title[i]
    meta(corpus[[i]],tag = "publication_type") <<- data$publication_type[i]
    meta(corpus[[i]],tag = "year") <<- data$publication_year[i]
  })
  # fpattern <- content_transformer(function(x, pattern) gsub(pattern, "", x))
  # complainCorpus <- tm_map(complainCorpus, fpattern, "z*")
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("SMART"))
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, stripWhitespace)
  strsplit_space_tokenizer <- function(x){
    unlist(strsplit(as.character(x), "[[:space:]]+"))
  }
  # control <- list(weighting = weightTf, tokenize= strsplit_space_tokenizer,
  #                 tolower = TRUE, removePunctuation = TRUE, removeNumbers = TRUE, stopwords = TRUE, stemming = TRUE,
  #                 dictionary = NULL, bounds = list(local = c(1, Inf)), wordLengths = c(3, Inf))
  control <- list(weighting = weightTf, tokenize= words,
                  tolower = TRUE, removePunctuation = TRUE, removeNumbers = TRUE, stopwords = TRUE, stemming = FALSE,
                  dictionary = NULL, bounds = list(local = c(1, Inf)), wordLengths = c(3, Inf))
  corpus_dtm <- DocumentTermMatrix(corpus,control)
  corpus_dtm
}
preprocess.keywords.corpus <- function(papers_keywords_df){
  data <- unique(papers_keywords_df)
  bi_matrix <- table(data$item_ut,tolower(data$author_keyword))
  corpus_dtm <- as.DocumentTermMatrix(bi_matrix,weighting = weightTf)
  corpus_dtm
}
#####
# community is like topic (from community to topic)
# 1.community_member is a bianry bipartite matrix
# 2.topic_member is a weighted community_member (also a bipartite matrix)
# 3.one doc belongs to many communities/topics
# 4.in some methods, one member belongs to many communities/topics
# 5.supported network form are edgelist, matrix and belongto_list
#####
# for return matrix
# return an topic-term or topic-edge bipartite matrix
# community-membership or called Topic-Member bipartite matrix
getTopicMemberBipartiteMatrix <- function(community_member_list, weight = "binary",graph=NULL,memberType="vertex"){
  bi_matrix <- matrix(data = 0,nrow = length(community_member_list),ncol = length(V(graph)),dimnames = c(list(1:length(community_member_list)),list(V(graph)$name)))
  for(i in 1:length(community_member_list)){
    if(memberType=="edge"){
      g <- delete.edges(graph,E(graph)[!(E(graph) %in% community_member_list[[i]])])
    }
    if(memberType=="vertex"){
      g <- delete.vertices(graph,names(V(graph))[!(names(V(graph)) %in% community_member_list[[i]])])
    }
    w <- switch(EXPR = weight,
                "binary" = rep(1,length(V(g))),
                "degree" = degree(g)/sum(degree(g)),
                "betweenness" = betweenness(g)/sum(betweenness(g)),
                "closeness" = closeness(g)/sum(closeness(g)),
                "evcent" = eigen_centrality(g)$vector/sum(eigen_centrality(g)$vector))
    bi_matrix[i,names(w)] <- w
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
    t(apply(doc_member,1,FUN = function(doc){doc/sqrt(sum(doc^2))})) %*% apply(topic_member,1,FUN = function(topic){topic/sqrt(sum(topic^2))})
  }
  doc_member <- as.matrix(doc_member[,colnames(doc_member) %in% colnames(topic_member)])
  topic_member <- as.matrix(topic_member)
  M <- switch(method,
              "Moore-Penrose" = calGeneralizedInverseMatrix(),
              "Transpose" = calTransposMatrix(),
              "similarity.cos" = calSimilarity.cos())
  if(scaling){
    M <- t(scale(t(M),center = F,scale = T)) 
  }
  M <- M/rowSums(M)
  M
}
getCommunityMemberBipartiteMatrix <- function(edge_community_df, weight = "degree"){
  result <- NULL
  result$doc_topic <- table(edge_community_df$node1,edge_community_df$cluster)/rowSums(table(edge_community_df$node1,edge_community_df$cluster))
  result$topic_term <- table(edge_community_df$cluster,edge_community_df$node2)/rowSums(table(edge_community_df$cluster,edge_community_df$node2))
  if(weight=="binary"){
    result$doc_topic[result$doc_topic>0] <- 1
    result$topic_term[result$topic_term>0] <- 1
  }
  return(result)
}
#####
# bipartite matrix and network plot report
#####
plotReport.bipartite.matrix <- function(corpus_dtm,topic_term,doc_topic,filename,path,drawNetwork=FALSE,coterm_graph=NULL,community_member_list=NULL,bipartite=F,edge_community_df=NULL){
  if(drawNetwork){
    if(bipartite){
      # network of topics (edges)
    }else{
      # network of topics (nodes)
      plotTopicNetworkReport(filename = filename,graph = coterm_graph,community_member_list,showNamesInPlot = TRUE,plotCommunity = TRUE,plotOverallTopics = TRUE,path = paste(path,"topic_term","network",sep = "/")) 
    }
  }
  # transpose = FALSE
  plotBipartiteMatrixReport(filename = filename,bi_matrix = corpus_dtm,path = paste(path,"document_term",sep = "/"),showNamesInPlot = FALSE, weightType = "tfidf", plotRowWordCloud = TRUE, plotWordCloud = TRUE, plotRowComparison = TRUE, plotRowDist = TRUE, plotModules = FALSE)
  plotBipartiteMatrixReport(filename = filename,bi_matrix = topic_term,path = paste(path,"topic_term",sep = "/"),showNamesInPlot = FALSE, weightType = "tf", plotRowWordCloud = TRUE, plotWordCloud = TRUE, plotRowComparison = TRUE, plotRowDist = TRUE, plotModules = FALSE)
  plotBipartiteMatrixReport(filename = filename,bi_matrix = doc_topic,path = paste(path,"doc_topic",sep = "/"),showNamesInPlot = FALSE, weightType = "tf", plotRowWordCloud = TRUE, plotWordCloud = TRUE, plotRowComparison = TRUE, plotRowDist = TRUE, plotModules = FALSE)
  # transpose = TRUE
  plotBipartiteMatrixReport(filename = filename,bi_matrix = corpus_dtm,transpose = TRUE,path = paste(path,"document_term",sep = "/"),showNamesInPlot = FALSE, weightType = "tfidf", plotRowWordCloud = FALSE, plotWordCloud = FALSE, plotRowComparison = FALSE, plotRowDist = TRUE, plotModules = FALSE)
  plotBipartiteMatrixReport(filename = filename,bi_matrix = topic_term,transpose = TRUE,path = paste(path,"topic_term",sep = "/"),showNamesInPlot = FALSE, weightType = "tf", plotRowWordCloud = FALSE, plotWordCloud = FALSE, plotRowComparison = FALSE, plotRowDist = TRUE, plotModules = FALSE)
  plotBipartiteMatrixReport(filename = filename,bi_matrix = doc_topic,transpose = TRUE,path = paste(path,"doc_topic",sep = "/"),showNamesInPlot = FALSE, weightType = "tf", plotRowWordCloud = FALSE, plotWordCloud = FALSE, plotRowComparison = FALSE, plotRowDist = TRUE, plotModules = FALSE)
}
#####
# document tagging test for doc_topic
#####
doc_topic.taggingtest <- function(doc_topic,papers_tags_df=NULL,filename,path,LeaveOneOut = FALSE){
  taggingtest_doc_topic <- cbind(item_ut=rownames(doc_topic),as.data.frame(doc_topic))
  taggingtest_doc_sc <- unique(papers_tags_df[,c("item_ut","subject_category")])
  taggingtest_data <- merge(taggingtest_doc_topic, taggingtest_doc_sc)
  # plot report
  doc.tagging.test(taggingtest_data = taggingtest_data,filename = filename,path = paste(path,"taggingtest",sep = "/"),LeaveOneOut = LeaveOneOut)
}
comm_member.communitytest <- function(community_member_list,bi_data_df,coterm_graph,papers_tags_df){
  # get community_term from community_member_list
  community_term <- getTopicMemberBipartiteMatrix(community_member_list,weight = "binary")
  # get member_tag_df from doc_term and doc_tag
  df_doc_tag <- unique(papers_tags_df[,c("item_ut","subject_category")])
  member_tag_df <- merge(bi_data_df,df_doc_tag)[,c("author_keyword","subject_category")]
  # calculate community test indices
  communitycoverage<-calcommunity.coverage(community_term)
  overlapcoverage<-caloverlap.coverage(community_term)
  communityquality<-calcommunity.quality(community_term,coterm_graph)
  overlapquality<-caloverlap.number.quality(community_member_list,member_tag_df)
  return(c(communitycoverage,overlapcoverage,communityquality,overlapquality))
}
#####
# required functions
#####
source("code/functions.R")
