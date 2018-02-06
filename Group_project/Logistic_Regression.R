library(leaps)
library(bestglm)
library(glmnet)
data = read.csv("bank-full.csv",sep=";")
attach(data)
summary(data)


data$y = ifelse(y == "yes" , 1 , 0)

model = glm(y~., data=data, family=binomial)
summary(model)


Mat = model.matrix(y~., data=data)
lambda = 10^(-5:0)

train = data[1:35000,]
test = data[35001:length(data[,1]),]

xtrain = model.matrix(y~.,train)[,-1]
ytrain = train$y

xtest = model.matrix(y~.,test)[,-1]
ytest = test$y


##' LASSO and Ridge Regressions
##' 

##' LASSO

lassoReg = cv.glmnet(xtrain, ytrain, family="binomial", alpha=1)
summary(lassoReg)
lassoReg["lambda.min"]
coef(lassoReg)
print(lassoReg)

# Predictions
lassoPred = predict(lassoReg, xtest, type="response")
lassoM = mean( (test$y - lassoPred)^2 )
lassoM
lassoRMSE = sqrt(lassoM)
lassoRMSE



##' Ridge

ridgeReg = cv.glmnet(xtrain, ytrain, family="binomial", alpha=0)
summary(ridgeReg)
ridgeReg["lambda.min"]
coef(ridgeReg)

# Prediction
ridgePred = predict(ridgeReg, xtest, type="response")
ridgeM = mean( (test$y - ridgePred)^2 )
ridgeM
ridgeRMSE = sqrt(ridgeM)
ridgeRMSE


ridgePred
summary(ridgePred)

# __________

##' Cross validation ad ROC

library(pROC)
library(caret)

#Create CV folds
fold <- createFolds(data$y, k = 10, list = FALSE)
data <- cbind(data,fold)

#Prediction model
predLasso = rep(NA, times = length(data$y))
predRidge = rep(NA, times = length(data$y))
for (i in 1:10){
  trainCV = data[data$fold != i,]
  testCV = data[data$fold == i,]
  xtrainCV = model.matrix(y~., data = trainCV)
  yCV = trainCV$y
  xtestCV = model.matrix(y~., testCV)
  
  lassoRegCV = cv.glmnet(xtrainCV, yCV, family="binomial", alpha=1)
  predLasso[data$fold==i] = predict(lassoRegCV, xtestCV, type="response")
  
  ridgeReg = cv.glmnet(xtrainCV, yCV, family="binomial", alpha=0)
  predRidge[data$fold==i] = predict(ridgeReg, xtestCV, type="response")
  
  print(i)
  
}

#Compute results
rLasso = roc(data$y, predLasso, direction="<")
rRidge = roc(data$y, predRidge, direction="<")

plot(rLasso,col="red", lwd=2, main="ROC")
lines(rRidge,col="blue", lwd=2, main="ROC")


classificationTableLasso = table(truth=data$y,predict=predLasso)
classificationTableLasso
classificationTableRidge = confusionMatrix(table(truth=data$y,predict=predRidge))
classificationTableRidge

auc(rLasso)
auc(rRidge)



#____________

##' Final Model

x = model.matrix(y~., data)[,-1]

ModelLogReg = cv.glmnet(x, data$y, family="binomial", alpha=0)
summary(ModelLogReg)
ModelLogReg["lambda.min"]
coef(ModelLogReg)


Pred = predict(ModelLogReg, x, type="response")
Pred_thr = Pred
Profit = c()
for (t in 10:300) {
  Pred_thr[Pred>(t/1000)] = 1
  Pred_thr[Pred<=t/1000] = 0
  classificationTable = table(truth=data$y,predict=Pred_thr)
  Profit[t-9] = classificationTable["1","1"]*78.5 - classificationTable["0","1"]*1.5
}
Profit
which.max(Profit)
Profit[which.max(Profit)]