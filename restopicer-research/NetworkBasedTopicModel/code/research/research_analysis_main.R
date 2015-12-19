rm(list = ls(envir = globalenv()))
options(encoding = "UTF-8")
source("code/methods.R")

load(file = "rdata/research_2011_2013.RData")
papers_tags_df <- researchPapersSubjectCategory
bi_data_df <- researchPapersKeywords
df_doc_tag <- unique(papers_tags_df[,c("item_ut","subject_category")])
member_tag_df <- merge(bi_data_df,df_doc_tag)[,c("author_keyword","subject_category")]

load("rdata/research_2011_2013_again/cutat_th=0.6_th=0.15.RData")
community_node_list <- lapply(result_linkcomm.percolation.edge_evcent_cos$community_member_list,FUN = function(x,coterm_g){
  g <- delete.edges(coterm_g,E(coterm_g)[!(E(coterm_g) %in% x)])
  g <- delete.vertices(g,names(V(g))[(names(V(g)) %in% names(V(g))[degree(g)==0])])
  names(V(g))
},coterm_g=result_linkcomm.percolation.edge_evcent_cos$coterm_graph)
community_term <- getTopicMemberBipartiteMatrix(community_member_list=result_linkcomm.percolation.edge_evcent_cos$community_member_list, weight = "binary",graph=result_linkcomm.percolation.edge_evcent_cos$coterm_graph,memberType="edge")

ncol(result_linkcomm.percolation.edge_evcent_cos$doc_topic)
calcommunity.entropy(doc_topic = result_linkcomm.percolation.edge_evcent_cos$doc_topic)
calcommunity.quality(comm_member = community_term,coterm_g = result_linkcomm.percolation.edge_evcent_cos$coterm_graph)
caloverlap.quality(community_member_list = community_node_list,member_tag_df = member_tag_df)
caloverlap.number.quality(community_member_list = community_node_list,member_tag_df)
calcommunity.coverage(community_term,trival_th = 100)
caloverlap.coverage(community_term,trival_th = 100)
calPartitionDensity(community_node_list,community_edge_list = result_linkcomm.percolation.edge_evcent_cos$community_member_list,coterm_g = NULL,type = "edge")
