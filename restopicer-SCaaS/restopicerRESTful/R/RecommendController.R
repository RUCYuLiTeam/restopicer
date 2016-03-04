simpleWeightControl <- function(mission_round=1){
  #summary_w <- exp(1-mission_round)/(1+exp(1-mission_round))
  #summary_w <- dexp(mission_round, rate = 1, log = FALSE)
  summary_w <- dweibull(mission_round, shape = 0.8, scale = 1, log = FALSE)
  if(mission_round==1){
    quality_w <- (1 - summary_w)/2
    fresh_w <- (1 - summary_w)/2
    explore_w <- 0
    preference_w <- 0
  }else if(mission_round>=2){
    quality_w <- (1 - summary_w)/4
    fresh_w <- (1 - summary_w)/4
    explore_w <- (1 - summary_w)/4 + ifelse(rbinom(1, 1, 1/(mission_round-1)^2)==1,1,-1) * abs((1 - summary_w)/4 - runif(1, min = 0, max = (1 - summary_w)/2))
    preference_w <- (1 - summary_w)/2 - explore_w 
  }
  list(preference_w=preference_w,
       explore_w=explore_w,
       quality_w=quality_w,
       summary_w=summary_w,
       fresh_w=fresh_w)
}