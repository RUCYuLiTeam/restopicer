# before percolation and cutat
#load(file = "rdata/tmp/lc.RData")
#length(lc$clusters)
# after cutat=0.5
#lc_cutat <- newLinkCommsAt(lc, cutat = 0.5)
#length(lc_cutat$clusters)
# after percolation
load(file = "rdata/research_2011_2013/result_linkcomm_th=0.15_percolation.RData")
load(file = "rdata/research_2011_2013_again/cutat_th=0.6_th=1.RData")
length(result_linkcomm.percolation_evcent_cos$community_member_list)
# analysis
# x <- result_linkcomm.percolation_evcent_cos$community_member_list[lapply(result_linkcomm.percolation_evcent_cos$community_member_list, function(x){length(x)})>20]
# length(x)
# x <- result_linkcomm.percolation_evcent_cos$topic_term[lapply(result_linkcomm.percolation_evcent_cos$community_member_list, function(x){length(x)})>15,]
# 
# plot(hclust(dist(topic_term[sort(colSums(doc_topic),decreasing = T)[1:100],],method = "euclidean")),labels = FALSE, hang = -1)

filename=result_linkcomm.percolation_evcent_cos$model$parameter
topic_term=result_linkcomm.percolation_evcent_cos$topic_term[lapply(result_linkcomm.percolation_evcent_cos$community_member_list, function(x){length(x)})>20,]
topic_term=topic_term[c(3,5,6,7,8,9,10,11,13,14),]
topic_term=result_linkcomm.percolation.edge_evcent_cos$topic_term[lapply(result_linkcomm.percolation.edge_evcent_cos$community_member_list, function(x){length(x)})>50,]
topic_term=topic_term[c(4,6,12,22,23,24,34,38,40,41,46,54,61,69,70,71,72,91,93,94,104,113,115,116,118),]
tmp <- hclust(dist(topic_term,method = "minkowski",p = 1),method = "ward.D")
plot(tmp,labels = FALSE, hang = -1)

topic_term_new<- t(sapply(split(names(cutree(tmp,k = 100)),cutree(tmp,k = 100),drop = T), function(x,topic_term){
  colMeans(topic_term[x,])
},topic_term=topic_term))
doc_topic <- getDocTopicBipartiteMatrix(doc_member = corpus_dtm,topic_member = topic_term_new,method = doc_topic_method)
doc_topic <- na.omit(doc_topic)
#doc_topic=result_linkcomm.percolation.edge_evcent_cos$doc_topic[,as.integer(row.names(topic_term))]
#topic_term=topic_term[sort(col_sums(doc_topic),decreasing = T),]
path="output/analysis"
plotBipartiteMatrixReport(filename = "k=2",bi_matrix = topic_term_new,path = paste(path,"topic_term_2",sep = "/"),showNamesInPlot = FALSE, weightType = "tf", plotRowWordCloud = TRUE, plotWordCloud = F, plotRowComparison = TRUE, plotRowDist = TRUE, plotModules = FALSE)

# for paper
filename=result_linkcomm.percolation_evcent_cos$model$parameter
mytopic<- which(lapply(result_linkcomm.percolation_evcent_cos$community_member_list, function(x){length(x)})>20)[c(3,5,6,7,8,9,10,11,13,14)]
doc_topic=result_linkcomm.percolation_evcent_cos$doc_topic[,mytopic]
doc_topic=doc_topic[rowSums(doc_topic)!=0,]
apply(doc_topic, 1, function(x){which.max(x)})
plotBipartiteMatrixReport(filename = filename,bi_matrix = doc_topic,path = paste(path,"doc_topic",sep = "/"),showNamesInPlot = FALSE, weightType = "tf", plotRowWordCloud = F, plotWordCloud = F, plotRowComparison = F, plotRowDist = TRUE, plotModules = FALSE)

topic=10
id_list=which(apply(doc_topic, 1, function(x){which.max(x)})==topic)
doc_topic[id_list,topic][which.max(doc_topic[id_list,topic])]
id_list[which.max(doc_topic[id_list,topic])]

ids <- c(1109,1105,487,580,414,981,1023,834,602,140)
ids <- c(1109,1105,487,580,414,674,1023,834,602,140)
ids <- c(1109,1105,487,580,414,674,1023,897,602,140)
dts=doc_topic[ids,]
dts=doc_topic[c(414,1023,1109,1105,487,580,674,897,602,id_list),]
plotBipartiteMatrixReport(filename = filename,bi_matrix = dts,path = paste(path,"doc_topic",sep = "/"),showNamesInPlot = FALSE, weightType = "tf", plotRowWordCloud = F, plotWordCloud = F, plotRowComparison = F, plotRowDist = TRUE, plotModules = FALSE)

# web of science category
