preferenceOnlyWeightControl <- function(mission_round=1){
  exploration_w <- 0
  exploitation_w <- 1 - exploration_w
  list(exploitation_relevent_w=exploitation_w/2,
       exploitation_rating_w=exploitation_w/2,
       exploration_learn_w=0,
       exploitation_quality_w=0,
       exploration_summary_w=0,
       exploration_fresh_w=0,
       exploration_w=exploration_w)
}
simpleHybridWeightControl <- function(mission_round=1,mu_explore=1/2){
  exploration_w <- mu_explore + ifelse(rbinom(1, 1, 1/(log(mission_round)+1))==1,1,-1) * runif(1, min = 0, max = 1-mu_explore)
  exploitation_w <- 1 - exploration_w
  if(mission_round <= 1){
    exploitation_relevent_w <- exploitation_w
    exploitation_rating_w <- 0
    exploitation_quality_w <- exploitation_w/3
    exploration_learn_w <- 0
    exploration_summary_w <- exploration_w/3
    exploration_fresh_w <- exploration_w/3
  }else{
    exploitation_relevent_w <- exploitation_w/2
    exploitation_rating_w <- exploitation_w/2
    exploitation_quality_w <- exploitation_w/4
    exploration_fresh_w <- exploration_w/4
    exploration_learn_w <- exploration_w/4
    exploration_summary_w <- exploration_w/4
  }
  list(exploitation_relevent_w=exploitation_relevent_w,
       exploitation_rating_w=exploitation_rating_w,
       exploration_learn_w=exploration_learn_w,
       exploitation_quality_w=exploitation_quality_w,
       exploration_summary_w=exploration_summary_w,
       exploration_fresh_w=exploration_fresh_w,
       exploration_w=exploration_w)
}
hybridWeightControl <- function(mission_round=1){
  exploration_w <- 1/2 + ifelse(rbinom(1, 1, 1/(log(mission_round)+1))==1,1,-1) * runif(1, min = 0, max = 1/2)
  exploitation_w <- 1 - exploration_w
  if(mission_round==1){
    exploitation_relevent_w <- exploitation_w
    exploitation_rating_w <- 0
    exploration_learn_w <- 0
    exploration_quality_w <- exploration_w/3
    exploration_summary_w <- exploration_w/3
    exploration_fresh_w <- exploration_w/3
  }else if(mission_round>=2&&exploration_w>3*exploitation_w){
    exploitation_relevent_w <- exploitation_w/2
    exploitation_rating_w <- exploitation_w/2
    exploration_learn_w <- exploration_w/4
    exploration_quality_w <- exploration_w/4
    exploration_summary_w <- exploration_w/4
    exploration_fresh_w <- exploration_w/4
  }else{
    exploitation_relevent_w <- exploitation_w/3
    exploitation_rating_w <- exploitation_w/3
    exploration_fresh_w <- exploitation_w/3
    exploration_learn_w <- exploration_w/3
    exploration_quality_w <- exploration_w/3
    exploration_summary_w <- exploration_w/3
  }
  list(exploitation_relevent_w=exploitation_relevent_w,
       exploitation_rating_w=exploitation_rating_w,
       exploration_learn_w=exploration_learn_w,
       exploration_quality_w=exploration_quality_w,
       exploration_summary_w=exploration_summary_w,
       exploration_fresh_w=exploration_fresh_w)
}
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
