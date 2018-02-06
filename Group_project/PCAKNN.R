library(readr)
library(factoextra)
library(class)
library(pROC)
library(caret)
library(MLmetrics)
bank_full <- read.csv("bank-full.csv",sep=";")

#PCA

bank_full2 = model.matrix(y~., data=bank_full)
bank_full2 <- bank_full2[,-1]

res.pca <- prcomp(bank_full2,scale=TRUE)

res.var=res.pca$sdev^2
pve=res.var/sum(res.var)
plot(cumsum(pve),col="red",type="b",pch=19,xlab="Principal Component",ylab="Cumulative Proportion of Variance Explained")

fviz_eig(res.pca)

fviz_pca_ind(res.pca,geom = c("point"),habillage=bank_full$y,palette=c("grey","red"))


#KNN

bank_full_scaled = scale(x=bank_full2, center = TRUE, scale = TRUE)

#Create CV folds
fold <- createFolds(bank_full$y, k = 10, list = FALSE)
bank_full <- cbind(bank_full,fold)

k_score = rep(0,10)

for (k in 1:10){
  
  score = rep(0,10)
  
  for (i in 1:10)
  {
    xtrain = bank_full_scaled[bank_full$fold != i,]
    xtest = bank_full_scaled[bank_full$fold == i,]
    
    ytrain = bank_full$y[bank_full$fold != i]
    ytest = bank_full$y[bank_full$fold == i]
    
    pred = knn(xtrain, xtest,ytrain,k)
    print(mean(pred == ytest))
    
    print(F1_Score(ytest, pred))
    score[i] = F1_Score(ytest, pred)
  
  }
  k_score[k] = mean(score)
}



