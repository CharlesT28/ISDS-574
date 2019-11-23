
rm(list=ls()); gc()
setwd('C:/Users/btransong/Downloads')
dat = read.csv('final dataset.csv', stringsAsFactors=T, head=T)
new_data = dat[,!(names(dat) %in% c('X','id','name','host_id','host_name','latitude','longitude','last_review'))]

set.seed(1)

n.train = floor(nrow(new_data)*.6)

id.train = sample(1:nrow(new_data), n.train)
id.test = setdiff(1:nrow(new_data), id.train)

Xtrain = new_data[id.train,2:ncol(new_data)]
Xtest = new_data[id.test,2:ncol(new_data)]
ytrain = new_data[id.train,1]
ytest = new_data[id.test,1]

Xtrain[, -c(6)] <- scale(Xtrain[, -c(6)])
Xtest[, -c(6)] <- scale(Xtest[, -c(6)])


##### functions for knn prediction #######
library(FNN)
one.pred = function(xnew, xtrain, ytrain, k, algorithm) {
  ind = knnx.index(xtrain, matrix(xnew, 1), k=k, algorithm=algorithm)
  mean(ytrain[ind])
}

knn.predict = function(Xtrain, ytrain, Xtest, k=5, algorithm = 'kd_tree') {
  ypred = apply(Xtest, 1, one.pred, xtrain = Xtrain, ytrain = ytrain, k=k, algorithm=algorithm)
  return(ypred)
}
library(dplyr)
knn.predict.bestK = function(Xtrain, ytrain, Xtest, ytest, k.grid = 1:20, algorithm='kd_tree') {
  fun.tmp = function(x) {
    yhat = knn.predict(Xtrain, ytrain, Xtest, k = x, algorithm=algorithm) # run knn for each k in k.grid
    rmse = (yhat - ytest)^2 %>% mean() %>% sqrt()
    return(rmse)
  }
  ## create a temporary function (fun.tmp) that we want to apply to each value in k.grid
  error = unlist(lapply(k.grid, fun.tmp))
  out = list(k.optimal = k.grid[which.min(error)], error.min = min(error))
  return(out)
}
#################################################


obj = knn.predict.bestK(Xtrain, ytrain, Xtest, ytest, k.grid = 1:18) 
obj

yhat = knn.predict(Xtrain, ytrain, Xtest, k = obj$k.optimal)
plot(ytest, yhat)

myRMSE = function(yhat.vec, ytest.vec) {
  er = yhat.vec - ytest.vec
  ntest = length(er)
  rmse = sqrt( mean(er^2) )
  return(rmse)
}

myRMSE(yhat, ytest)