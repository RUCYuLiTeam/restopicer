# before percolation and cutat
#load(file = "rdata/tmp/lc.RData")
#length(lc$clusters)
# after cutat=0.5
#lc_cutat <- newLinkCommsAt(lc, cutat = 0.5)
#length(lc_cutat$clusters)
# after percolation
load(file = "rdata/research_2011_2013/result_linkcomm_th=0.15_percolation.RData")
length(result_linkcomm.percolation_evcent_cos$community_member_list)
# analysis
x <- result_linkcomm.percolation_evcent_cos$community_member_list[lapply(result_linkcomm.percolation_evcent_cos$community_member_list, function(x){length(x)})>20]
length(x)
x <- result_linkcomm.percolation_evcent_cos$topic_term[lapply(result_linkcomm.percolation_evcent_cos$community_member_list, function(x){length(x)})>15,]

filename=result_linkcomm.percolation_evcent_cos$model$parameter
topic_term=result_linkcomm.percolation_evcent_cos$topic_term[lapply(result_linkcomm.percolation_evcent_cos$community_member_list, function(x){length(x)})>20,]
topic_term[c(3,5,6,7,8,9,10,11,13,14),]
path="output/analysis"
plotBipartiteMatrixReport(filename = filename,bi_matrix = topic_term,path = paste(path,"topic_term",sep = "/"),showNamesInPlot = FALSE, weightType = "tfidf", plotRowWordCloud = TRUE, plotWordCloud = F, plotRowComparison = TRUE, plotRowDist = TRUE, plotModules = FALSE)
