elasticNetPreferenceEval <- function(enetmodel,predict_new_docs){
  #fits <- predict.enet(object = enetmodel, newx = predict_new_doc, s = dim(enetmodel$beta.pure)[1], type = "fit",mode = "step")
  fits <- predict.enet(object = enetmodel, newx = predict_new_docs, s = 0.5, type = "fit",mode = "fraction")
  fits$fit
}
