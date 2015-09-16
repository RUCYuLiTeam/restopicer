library(entropy)
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

# df_bi_data <- data[,c("item_ut","author_keyword")]
# df_bi_data$author_keyword <- tolower(df_bi_data$author_keyword)
# df_bi_data <- unique(df_bi_data)
# df_doc_tag <- unique(demoPapersSubjectCategory[,c("item_ut","subject_category")])
caloverlap.quality<- function(comm_member,df_bi_data,df_doc_tag){
  member_comm_list <- apply(comm_member,MARGIN = 2,FUN = function(v){
    names(v[v!=0])
  })
  v.model <- unlist(lapply(member_comm_list,FUN = length))
  v.model <- v.model[order(names(v.model),decreasing = F)]
  member_tag_df <- merge(df_bi_data,df_doc_tag)[,2:3]
  v.real <- unlist(lapply(split(member_tag_df,member_tag_df[,1]),FUN = nrow))
  v.real <- v.real[order(names(v.real),decreasing = F)]
  # cal the mutual information
  entropy::mi.plugin(matrix(c(v.model,v.real),nrow = 2,ncol = length(v.real)),unit = "log2")
}