# http://www.cnblogs.com/EE-NovRain/p/3810737.html
# Assuming that most changes in estimates are for the better, an item that causes
# many estimates to change will result in the improvement of many estimates,
# and is considered useful
activeExploreEval <- function(enetmodel,#learn a preference approximation function based on the current training set
                              new_doc_i,test_docs,train_docs,train_rating){
  ### extract the coefficient vector with L1 norm fraction=0.5
  #coef <- predict.enet(enetmodel, s=0.25, type="coef", mode="fraction")
  #sum(coef$coefficients!=0)
  test_fits <- predict.enet(object = enetmodel, newx = test_docs, s = 0.5, type = "fit",mode = "fraction")
  # generate new training set : add a hypothetical training point
  new_train_docs <- rbind(train_docs,test_docs[new_doc_i,])
  # final G_change
  G_change <- 0
  # for each possible rating from 1 to 5
  for(new_rating in 1:5){
    # learn a new preference approximation function based on the new training set
    new_enetmodel <- enet(x = I(new_train_docs),y = c(train_rating,new_rating),lambda=0.5,normalize = F,intercept = T)
    #plot(new_enetmodel)
    # for each unrated docs
    new_test_fits <- predict.enet(object = new_enetmodel, newx = test_docs, s = 0.5, type = "fit",mode = "fraction")
    # record and cal the differences between ratings estimates
    G_change <- G_change + sum((test_fits$fit-new_test_fits$fit)^2)
  }
  G_change/5
}