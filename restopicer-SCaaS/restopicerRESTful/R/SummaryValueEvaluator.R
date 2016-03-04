topicEntropySummaryEval <- function(z){
  -sum(z*log(z))
}