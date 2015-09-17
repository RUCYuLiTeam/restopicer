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