library(readr)
library(class)
library(pROC)
library(caret)
library(leaps)
library(glmnet)
library(rpart)
library(ROCR)
library(MLmetrics)

setwd("D:/Columbia/Courses/[Fall 1] Business Analytics/Group Project")

#Loading the dataset
set.seed(4650)
data = read.table("bank-full.csv",header=TRUE,sep=";")
data$y = ifelse(data$y == "yes", 1, 0)
summary(data)

#Create CV folds
fold <- createFolds(data$y, k = 10, list = FALSE)
data <- cbind(data,fold)

#Prediction model
predLasso = rep(NA, times = length(data$y))
predRidge = rep(NA, times = length(data$y))
predKnn = rep(NA, times = length(data$y))
predTree = rep(NA, times = length(data$y))

for (i in 1:10){
  # separate train and test data for each fold
  train = data[data$fold != i,]
  test = data[data$fold == i,]
  
  # transform data into matrix for some models
  xtrain = model.matrix(y~., data = train)[,-1]
  ytrain = train$y
  xtest = model.matrix(y~., test)[,-1]
  
  
  # LASSO logistic regression
  lassoReg = cv.glmnet(xtrain, ytrain, family="binomial", alpha=1)
  predLasso[data$fold==i] = predict(lassoReg, xtest, type="response")
  
  
  # Ridge logistic regression
  ridgeReg = cv.glmnet(xtrain, ytrain, family="binomial", alpha=0)
  predRidge[data$fold==i] = predict(ridgeReg, xtest, type="response")
  
  
  # Knn prediction
  x = model.matrix(y~.-fold, data=data)[,-1]
  data_scaled = scale(x, center = TRUE, scale = TRUE)
  train_scaled = data_scaled[data$fold != i,]
  test_scaled = data_scaled[data$fold == i,]
  
  knn_temp = knn(train_scaled, test_scaled, ytrain, k=7, prob=TRUE)
  knn_temp = ifelse( knn_temp == 1, attributes(knn_temp)$prob, 1-attributes(knn_temp)$prob )
  predKnn[data$fold==i] = knn_temp
  

  # Decision tree
  tree_bank = rpart(train$y~., data=train, parms=list(split="information", prio=c(0.117,0.883)), control=rpart.control(minsplit=2, maxdepth=13, cp=0))
  tree_simple = prune(tree_bank, cp=tree_bank$cptable[which.min(tree_bank$cptable[,4]),1])
  
  tree_temp = predict(tree_simple, test)
  predTree[data$fold==i] = tree_temp
  
  print(i)
  
}


#Compute results
rLasso = roc(data$y, predLasso, direction="<")
rRidge = roc(data$y, predRidge, direction="<")
rKnn = roc(data$y, predKnn, direction="<")
rTree = roc(data$y, predTree, direction="<")

plot(rLasso,col="red", lwd=2, main="ROC")
lines(rRidge,col="blue", lwd=2, main="ROC")
lines(rKnn,col="green", lwd=2, main="ROC")
lines(rTree,col="purple", lwd=2, main="ROC")


rLasso$auc
rRidge$auc
rKnn$auc
rTree$auc

