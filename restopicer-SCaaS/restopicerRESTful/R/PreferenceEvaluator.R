elasticNetPreferenceEval <- function(enetmodel,predict_new_docs){
  #fits <- predict.enet(object = enetmodel, newx = predict_new_doc, s = dim(enetmodel$beta.pure)[1], type = "fit",mode = "step")
  fits <- predict.enet(object = enetmodel, newx = predict_new_docs, s = 0.5, type = "fit",mode = "fraction")
  pred_fit <- fits$fit
  #fits <- predict.glmnet(object = enetmodel,newx = predict_new_docs,s=0.5,type = "link",exact = T)
  #fits
  delta <- (max(pred_fit)-min(pred_fit))/5
  pred_rate <- sapply(1:5,FUN = function(multiply){
    pred_fit <= (min(pred_fit) + multiply*delta)
  })
  rowSums(pred_rate)
}
