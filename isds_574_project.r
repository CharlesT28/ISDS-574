rm(list=ls()); gc()

setwd('C:/Users/charlesturner/Downloads')

dat = read.csv('newlistings.csv', head=T, stringsAsFactors=F) 

new_data = dat[,!(names(dat) %in% c('X','id','name','host_id','host_name','latitude','longitude','last_review'))]

set.seed(1) 

n.train = floor(nrow(new_data)*.6)

id.train = sample(1:nrow(new_data), n.train)

id.test = setdiff(1:nrow(new_data), id.train)

obj = lm(price ~ .,data = new_data[id.train, ])

summary(obj)

names(obj) 

obj$resid

plot(obj$resid)

obj$fitted

yhat = predict(obj, newdata = new_data[id.test, ])

length(yhat)

length(id.test)

par(mfrow=c(1,1))

plot(new_data[id.test, 'price'], yhat, xlab='Actual y', ylab='Fitted y')

abline(0, 1, col='red') # add a line with intercept 0 and slope 1; we want to see points around this line

require(hydroGOF)

rmse(new_data[id.test, 'price'], yhat) ## RMSE for test data

# normality (of the residual)
hist(obj$resid)

# linearity

plot(new_data$minimum_nights, new_data$price)

# Homoscedasticity
plot(obj$resid, obj$fitted)

library(MASS)

obj.null = lm(price ~ 1, dat = new_data[id.train, ])
obj.full = lm(price ~ ., dat = new_data[id.train, ]) 


obj1 = step(obj.null, scope=list(lower=obj.null, upper=obj.full), direction='forward')
summary(obj1) 

obj2 = step(obj.full, scope=list(lower=obj.null, upper=obj.full), direction='backward')
summary(obj2) 

obj3 = step(obj.null, scope=list(lower=obj.null, upper=obj.full), direction='both') 
summary(obj3)

library(leaps)
obj4 = regsubsets(Price ~ ., data = new_data[id.train, ], nvmax=20)
summary(obj4)



# forward
yhat1 = predict(obj1, newdata = new_data[id.test, ])
rmse(new_data[id.test, 'price'], yhat1) ## RMSE for test data

# backward
yhat2 = predict(obj2, newdata = new_data[id.test, ])
rmse(new_data[id.test, 'price'], yhat2)

# stepwise
yhat3 = predict(obj3, newdata = new_data[id.test, ])
rmse(new_data[id.test, 'price'], yhat3)


#####
library(rpart)

fit = rpart(price ~ ., method="anova", data=new_data)

yhat1 = predict(fit, newdata = new_data[id.test, ])

rmse(new_data[id.test, 'price'], yhat1)

summary(fit)