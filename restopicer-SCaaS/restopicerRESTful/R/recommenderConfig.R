hostname="222.29.196.230"
username="restopicer"
pwd="abc123"
getRecommender<-function(recommendername){
  cmpfun(
    switch(recommendername,
           noneRecommender=function(result_relevent,composite_N,...){
             result_relevent[1:min(length(result_relevent),composite_N)]
           },
           exploreHybridRecommend= exploreHybridRecommend,
           preferenceOnlyRecommend= preferenceOnlyRecommend,
           weightedHybridRecommend= weightedHybridRecommend,
           allotHybridRecommend= allotHybridRecommend
    ))
}
# recommender control
getRecommendController<-function(controllername){
  cmpfun(
    switch(controllername,
           simpleWeightControl=simpleWeightControl,
           hybridWeightControl=hybridWeightControl,
           simpleHybridWeightControl=simpleHybridWeightControl
    ))
}
# sub-measures in every recommender
data(result_LDA_abstarct_VEM, envir=environment())
data(pretrain_doc, envir=environment())
getPreferenceEvaluator<-function(name){
  cmpfun(
    switch(name,
           elasticNetPreferenceEval=elasticNetPreferenceEval,
           cosPreferenceEval=cosPreferenceEval
    ))
}
getQualityEvaluator<-function(name){
  cmpfun(
    switch(name,
           citedQualityEval=citedQualityEval,
           SCIImpactFactorEval=SCIImpactFactorEval
    ))
}
getSummaryValueEvaluator<-function(name){
  cmpfun(
    switch(name,
           topicEntropySummaryEval=topicEntropySummaryEval
    ))
}
getFreshEvaluator<-function(name){
  cmpfun(
    switch(name,
           yearDiffReciprocalFreshEval=yearDiffReciprocalFreshEval
    ))
}
getExploreValueEvaluator<-function(name){
  cmpfun(
    switch(name,
           activeExploreEval=activeExploreEval
    ))
}
