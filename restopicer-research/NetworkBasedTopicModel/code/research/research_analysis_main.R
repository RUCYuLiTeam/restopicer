rm(list = ls(envir = globalenv()))
options(encoding = "UTF-8")
source("code/methods.R")

# load(file = "rdata/research_2011_2013.RData")
# papers_tags_df <- researchPapersSubjectCategory
# bi_data_df <- researchPapersKeywords
# df_doc_tag <- unique(papers_tags_df[,c("item_ut","subject_category")])
# member_tag_df <- merge(bi_data_df,df_doc_tag)[,c("author_keyword","subject_category")]

load("rdata/research_2011_2013_again/cutat_th=0.6_th=1.RData")
load("rdata/research_2011_2013/result_infomap_40.RData")
community_node_list <- result_fastgreedy_evcent_cos$community_member_list
######
# community_node_list <- lapply(result_linkcomm.percolation.edge_evcent_cos$community_member_list,FUN = function(x,coterm_g){
#   g <- delete.edges(coterm_g,E(coterm_g)[!(E(coterm_g) %in% x)])
#   g <- delete.vertices(g,names(V(g))[(names(V(g)) %in% names(V(g))[degree(g)==0])])
#   names(V(g))
# },coterm_g=result_linkcomm.percolation.edge_evcent_cos$coterm_graph)
# community_node_list <- lapply(lc$clusters,FUN = function(x,coterm_g){
#   g <- delete.edges(coterm_g,E(coterm_g)[!(E(coterm_g) %in% x)])
#   g <- delete.vertices(g,names(V(g))[(names(V(g)) %in% names(V(g))[degree(g)==0])])
#   names(V(g))
# },coterm_g=lc$igraph)
#community_term <- getTopicMemberBipartiteMatrix(community_member_list=lc$clusters, weight = "binary",graph=lc$igraph,memberType="edge")
#community_term <- getTopicMemberBipartiteMatrix(community_member_list=community_node_list,graph=lc$igraph, weight = "binary")
#community_node_list <- result_linkcomm.percolation.edge_evcent_cos$community_node_list
community_term <- getTopicMemberBipartiteMatrix(community_member_list=community_node_list,graph=lc$igraph, weight = "binary")
community_term <- result_linkcomm.percolation.edge_evcent_cos$community_term
#ncol(result_linkcomm.percolation.edge_evcent_cos$doc_topic)
#result_linkcomm.percolation.edge_evcent_cos$entropy
#calcommunity.entropy(doc_topic = result_linkcomm.percolation.edge_evcent_cos$doc_topic)
#result_linkcomm.percolation.edge_evcent_cos$Qc
calcommunity.quality <- function(comm_member,coterm_g){
  coterm_matrix <- get.adjacency(coterm_g,type = "upper")
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
calcommunity.quality(comm_member = community_term,coterm_g = lc$igraph)
#caloverlap.quality(community_member_list = community_node_list,member_tag_df = member_tag_df)
#result_linkcomm.percolation.edge_evcent_cos$Qo
#caloverlap.number.quality(community_member_list = community_node_list,member_tag_df)
calcommunity.coverage(community_term,trival_th = 26)
caloverlap.coverage(community_term,trival_th = 26)
#calPartitionDensity(community_node_list,community_edge_list = result_linkcomm.percolation.edge_evcent_cos$community_member_list,coterm_g = NULL,type = "edge")
calcommunity.balance(community_term,trival_th = 15)
