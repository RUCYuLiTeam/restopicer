foldername <- "research_5yr"
load(paste("../NetworkBasedTopicModel/rdata",foldername,"result_LDA_abstarct_gibbs.RData",sep="/"))
result_LDA_abstarct_gibbs$topic_term

result_LDA_abstarct_gibbs$doc_topic


require(elasticnet)

data(diabetes)
attach(diabetes)
##fit the lasso model (treated as a special case of the elastic net)
object1 <- enet(x,y,lambda=0,normalize = F,intercept = T)
plot(object1)
##fit the elastic net model with lambda=1.
object2 <- enet(x,y,lambda=1,normalize = F,intercept = T)
plot(object2)
##early stopping after 50 LARS-EN steps
object3 <- enet(x,y,lambda=0.5,normalize = F,intercept = T)
plot(object3)
#model
object <- enet(x,y,lambda=0,normalize = F,intercept = T)
### make predictions at the values in x, at each of the
### steps produced in object
fits <- predict.enet(object, x, s=dim(object$beta.pure)[1], type="fit",mode="step")
coef <- predict.enet(object, x, s=dim(object$beta.pure)[1], type="coef",mode="step")
### extract the coefficient vector with L1 norm=2000
coef <- predict.enet(object, s=9, type="coef", mode="norm")
### extract the coefficient vector with L1 norm fraction=0.45
coef <- predict.enet(object, s=0.45, type="coef", mode="fraction")
detach(diabetes)
