# http://www.cnblogs.com/EE-NovRain/p/3810737.html
# Assuming that most changes in estimates are for the better, an item that causes
# many estimates to change will result in the improvement of many estimates,
# and is considered useful
activeExploreEval <- function(enetmodel,#learn a preference approximation function based on the current training set
                              new_doc_i,test_docs,train_docs,train_rating,sample_size=50){
  if(sample_size<length(train_rating)){
    train_sample <- sample(1:length(train_rating),size = sample_size,replace = F)
    train_docs <- train_docs[train_sample,]
    train_rating <- train_rating[train_sample]
  }
  ### extract the coefficient vector with L1 norm fraction=0.5
  #coef <- predict.enet(enetmodel, s=0.5, type="coef", mode="fraction")
  #sum(coef$coefficients!=0)
  test_fits <- predict.enet(object = enetmodel, newx = test_docs, s = 0.5, type = "fit",mode = "fraction")
  #test_fits <- predict.glmnet(object = enetmodel,newx = test_docs,s=0.5,type = "link",exact = T)
  # generate new training set : add a hypothetical training point
  new_train_docs <- rbind(train_docs,test_docs[new_doc_i,])
  new_train_ratings <- lapply(1:5,FUN = function(x){append(train_rating,x)})
  # final G_change
  G_change <- 0
  # for each possible rating from 1 to 5
  for(new_rating in new_train_ratings){
    # learn a new preference approximation function based on the new training set
    new_enetmodel <- enet(x = new_train_docs,y = new_rating,lambda=0.5,normalize = F,intercept = F)
    #new_enetmodel <- glmnet(x = new_train_docs,y = new_rating, alpha=0.5, intercept = F)
    #plot(new_enetmodel)
    # for each unrated docs
    new_test_fits <- predict(object = new_enetmodel, newx = test_docs, s = 0.5, type = "fit",mode = "fraction")
    #new_test_fits <- predict(object = new_enetmodel, newx = test_docs, s = 0.5, type = "link",exact = T)
    # record and cal the differences between ratings estimates
    G_change <- G_change + sum((test_fits$fit-new_test_fits$fit)^2)
    #G_change <- G_change + sum((test_fits-new_test_fits)^2)
  }
  G_change/5
}