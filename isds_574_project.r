rm(list=ls()); gc()

setwd('C:/Users/charlesturner/Downloads')

dat = read.csv('final dataset.csv', head=T, stringsAsFactors=F) 

new_data = dat[,!(names(dat) %in% c('X','price','id','name','host_id','host_name','latitude','longitude','last_review'))]

set.seed(1) 

n.train = floor(nrow(new_data)*.6)

id.train = sample(1:nrow(new_data), n.train)

id.test = setdiff(1:nrow(new_data), id.train)

obj = lm(logprice ~ .,data = new_data[id.train, ])

summary(obj)

names(obj) 

obj$resid

plot(obj$resid)

obj$fitted

yhat = predict(obj, newdata = new_data[id.test, ])

yhat_normal = exp(yhat)

length(yhat)

length(id.test)

par(mfrow=c(1,1))

plot(exp(new_data[id.test, 'logprice']), yhat_normal, xlab='Actual y', ylab='Fitted y')

abline(0, 1, col='red') # add a line with intercept 0 and slope 1; we want to see points around this line

require(hydroGOF)

rmse(exp(new_data[id.test, 'logprice']),exp(yhat)) ## RMSE for test data

plot(obj$resid, obj$fitted)

library(MASS)

obj.null = lm(logprice ~ 1, dat = new_data[id.train, ])
obj.full = lm(logprice ~ ., dat = new_data[id.train, ]) 

obj1 = step(obj.null, scope=list(lower=obj.null, upper=obj.full), direction='forward')
summary(obj1) 

obj2 = step(obj.full, scope=list(lower=obj.null, upper=obj.full), direction='backward')
summary(obj2) 

obj3 = step(obj.null, scope=list(lower=obj.null, upper=obj.full), direction='both') 
summary(obj3)

library(leaps)
obj4 = regsubsets(logprice ~ ., data = new_data[id.train, ], nvmax=20)
summary(obj4)

# forward
yhat1 = predict(obj1, newdata = new_data[id.test, ])
rmse(exp(new_data[id.test, 'logprice']), exp(yhat1)) ## RMSE for test data

# backward
yhat2 = predict(obj2, newdata = new_data[id.test, ])
rmse(exp(new_data[id.test, 'logprice']), exp(yhat2))

# stepwise
yhat3 = predict(obj3, newdata = new_data[id.test, ])
rmse(exp(new_data[id.test, 'logprice']), exp(yhat3))


##### CART

library(rpart)
library(rpart.plot)

fit = rpart(logprice ~ ., method="anova", data=new_data[id.train, ])

yhat1 = predict(fit, newdata = new_data[id.test, ])

rmse(exp(new_data[id.test, 'logprice']), exp(yhat1))

rpart.plot(fit, main = 'Full Tree')

summary(fit)

pfit = prune(fit, cp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

yhat_prune = predict(pfit, newdata = new_data[id.test, ])

rmse(exp(new_data[id.test, 'logprice']), exp(yhat_prune))

rpart.plot(pfit, main = 'Min Error Tree')

summary(pfit)

#### KNN

training_data = new_data[id.train, ]
testing_data = new_data[id.test, ]

Xtrain = training_data[,names(training_data) != "logprice"]
Xtest = testing_data[,names(testing_data) != "logprice"]
ytrain = new_data[id.train,6]
ytest = new_data[id.test,6]

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

#obj = knn.predict.bestK(Xtrain, ytrain, Xtest, ytest, k.grid = 1:11) 
#obj

yhat = knn.predict(Xtrain, ytrain, Xtest, k = 6)

plot(ytest, yhat)

myRMSE = function(yhat.vec, ytest.vec) {
  er = yhat.vec - ytest.vec
  ntest = length(er)
  rmse = sqrt( mean(er^2) )
  return(rmse)
}

myRMSE(exp(yhat), exp(ytest))
