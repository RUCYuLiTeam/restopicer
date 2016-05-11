topicEntropySummaryEval <- function(z){
  summary_words_matrix <- pretrain_doc$terms[,which(colnames(pretrain_doc$terms) %in% c("review","survey"))]
  summary_prob <- rowSums(summary_words_matrix)/sum(rowSums(summary_words_matrix))
  -sum(z*log(z)) + z %*% summary_prob
}
