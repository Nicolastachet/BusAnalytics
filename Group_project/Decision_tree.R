setwd("/Users/NicoTcht/Desktop/bank/")
data=read.csv('bank-full.csv', sep =";")

summary(data)
head(data)
dim(data)

data$result = ifelse(data$y == "yes", 1, 0)


### Decision tree
### Using rpart
library("rpart")
library("ROCR")
library("caret")
#Cross-validation k=10

#Create CV folds
fold <- createFolds(data$y, k = 4, list = FALSE)
df <- cbind(data,fold)

best_mse=list()
for (i in 4:13){#depth parameter
  mse = 0
  for (j in 1:4){#cross-val
    trainCV = df[df$fold != j,]
    testCV = df[df$fold == j,]
    tree_bank <- rpart(trainCV$y ~ . -fold-result, data = trainCV, parms = list(split = "information",prior = c(0.117,0.883)), control = rpart.control(minsplit = 2,maxdepth = i,cp = 0))
    tree_simple <- prune(tree_bank,cp = tree_bank$cptable[which.min(tree_bank$cptable[,4]),1])
    
    pred2 <- predict(tree_simple, testCV, type="class")
    pred2 = ifelse(pred2 == "yes", 1, 0)
    mse_rate = mean((pred2-testCV$result)^2)
    mse = mse + mse_rate
  }
  best_mse[[i-3]] = mse
}

## For maxdepth = 13 we have the lowest mse
predTree = rep(NA, times =length(data$y))
for (j in 1:4){#cross-val
  trainCV = df[df$fold != j,]
  testCV = df[df$fold == j,]
  tree_bank <- rpart(trainCV$y ~ . -result, data = trainCV, parms = list(split = "information",prior = c(0.117,0.883)), control = rpart.control(minsplit = 2,maxdepth = 13,cp = 0))
  tree_simple <- prune(tree_bank,cp = tree_bank$cptable[which.min(tree_bank$cptable[,4]),1])

  predTree[df$fold == j] = predict(tree_simple, testCV, type="prob")
}

pred = 1-predTree
rpart.plot(tree_simple)

r = roc(df$y, pred, direction="<")
plot(r,col="red", lwd=3, main="ROC")

#confusion for test set
pred2 <- predict(tree_simple, test, type="class")
table(truth = test$y, predict = pred2)




