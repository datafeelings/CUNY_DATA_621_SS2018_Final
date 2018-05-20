train_inp <- read.csv("https://raw.githubusercontent.com/datafeelings/CUNY_DATA_621_SS2018_Final/master/2_code/train_inp.csv")
test_inp <- read.csv("https://raw.githubusercontent.com/datafeelings/CUNY_DATA_621_SS2018_Final/master/2_code/test_inp.csv")

train_inp <- train_inp[complete.cases(train_inp),]

library(gbm)
#Regression Trees
#model using all variables
gbm.test1 <- test_inp[ , "price"]

gbm1 <- gbm(price ~ ., train_inp, distribution = "gaussian", n.trees = 10000, cv.folds = 5)

#model using variables that were rated as two in our variable evaluation 

train_inp2 <- train_inp[-c(3, 4:9, 14:16, 30, 33, 36:39, 42, 44)]
gbm2 <- gbm(price ~., train_inp2, distribution = "gaussian", n.trees = 10000, cv.folds = 5)

plot_predicts <-function(predicted_price, real_price, text, color){
  plot(x = real_price, y = predicted_price, xlab = "actual price",
       ylab = "predicted price", main = text,xlim= c(0,8), ylim= c(0,8), col = color)
  abline(a = 0, b = 1)
}

gbm.perf(gbm1)
gbm.perf(gbm2)

pretty.gbm.tree(gbm1)
pretty.gbm.tree(gbm2)

summary(gbm1)
summary(gbm2)

par(mar=c(1,1,1,1))
plot(gbm1, i=paste(summary(gbm1, plotit=FALSE)$var[[1]], "", sep=""))
plot(gbm1, i=paste(summary(gbm1, plotit=FALSE)$var[[2]], "", sep=""))
plot(gbm1, i=paste(summary(gbm1, plotit=FALSE)$var[[3]], "", sep=""))
yhat <- predict(gbm1, newdata = test_inp)#PREDICTED PRICES
sqrt(mean((yhat - gbm.test1)^2)) #RSME
mean(abs(yhat - gbm.test1)) #Mean Absolute Error
plot_predicts(yhat, gbm.test1, "Trees on Air BnB Validation", "red")


par(mar=c(1,1,1,1))
plot(gbm2, i=paste(summary(gbm2, plotit=FALSE)$var[[1]], "", sep=""))
plot(gbm2, i=paste(summary(gbm2, plotit=FALSE)$var[[2]], "", sep=""))
plot(gbm2, i=paste(summary(gbm2, plotit=FALSE)$var[[3]], "", sep=""))
yhat2 <- predict(gbm2, newdata = test_inp)
sqrt(mean((yhat2- gbm.test1)^2)) # RMSE
mean(abs(yhat2 - gbm.test1)) #Mean Absolute Error
plot_predicts(yhat2, gbm.test1, "Trees on Air BnB Validation", "red")
