# Assignment 3 - Nicolas Tachet - nt2479
### DATADOG_BARK ###
####################

setwd('/Users/NicoTcht/Desktop/ColumbiaUniv/Business_Analytics/Assign3')
datadog = read.csv('dog.csv')
names(datadog)
head(datadog)
attach(datadog)
tail(datadog)

#Question 1

############# Question b #############
set.seed(4650)
train_ind=sample(1:nrow(datadog),0.75*nrow(datadog))
train <- datadog[train_ind,]
validation <- datadog[-train_ind,] 
summary(train)

############# Question c #############
# Predict using a tree_score threshold
#Benchmark Profit on the train test
BenchmarkProfit = 858*0.5 + (1611)*(-1) #-1182 $ if we send to everybody on the train

#Let's try with a threshold equal to 100
s = 100
tree_scorePred = train$tree_score > s
tree_scorePred = ifelse(tree_scorePred,1,0)
# table to see error rates
classficationTable = table(truth=train$dog, 
      predict= tree_scorePred)
revenue =  c(c(0,0),c(-1,0.5))
sum(classficationTable * revenue)
# Profit = -298$ We improve our profit (but it is still negative) 

revenuePerThreshold = vector("numeric",126)
for (s in 0:125)
{
  tree_scorePred = train$tree_score > s + 15
  # table to see error rates
  classficationTable = table(truth=train$dog, 
                             predict= tree_scorePred)
  revenuePerThreshold[s] = sum(classficationTable * revenue)
}
plot(15:140,revenuePerThreshold,pch = 15, xlab = "Threshold", ylab= "Profit")

###### The company is always in deficit on the train data, the best deficit is 0$
###### i.e when we send nothing to the customer (no gain, no loss)

############# Question d #############
revenuePerThreshold = vector("numeric",126)
for (s in 0:125)
{
  tree_scorePred = validation$tree_score > s + 15
  # table to see error rates
  classficationTable = table(truth=validation$dog, 
                             predict= tree_scorePred)
  print(classficationTable)
  revenuePerThreshold[s] = sum(classficationTable * revenue)
}
plot(0:125,revenuePerThreshold,pch = 15, xlab = "Threshold", ylab = "Profit (validation)")

### It performs worse than on the test data
### (and we still have 0$ profit if don't send any pamphlet)

############# Question e: Perfect classifier #############
## there are 1148 dog owner in the dataset
## Thus max Profit = 1148*(5%)*(30$) - 1148$ = 574$

############# Question f #############
## Using a logistic regression 
lgfit = glm(train$dog ~.-X, data = train, family = binomial)
summary(lgfit)

############# Question g #############
## logistic regression classifier
lgPrediction = predict(lgfit,newdata = train, type = "response")

#Transform the predicted probability to a decision and test it
revenuePerProba = vector("numeric",70)
for (p in seq(0.1,0.8,by=0.01)) 
{
  probaStar = p
  ind = 100*p
  print(ind)
  # table to see error rates
  lgDecision = ifelse(lgPrediction > probaStar,1,0)
  classficationTable = table(truth=train$dog, 
                             predict=lgDecision )
  print(classficationTable)
  revenuePerProba[ind] = sum(classficationTable * revenue)
}
plot(seq(0.1,0.80,by=0.01),revenuePerProba[10:80],pch = 15, xlab = "p*", ylab="Profit")
## MaxProfit = 26.5$ for confusion matrix:
#         predict
# truth    0    1
#    No 1596   15
#   Yes  775   83

############# Question h: decision tree #############
library(tree)
tree_datadog = tree(train$dog~.-X-pub_dist, data=train)
summary(tree_datadog)

plot(tree_datadog)
text(tree_datadog,pretty=0)
tree_datadog
pred1 <- predict(tree_datadog, train, type="class")
table(truth = train$dog, predict = pred1)
# Profit = -18$ within the training data (deficit)
# Confusion Matrix
#        predict
# truth    No  Yes
#    No  1493  118
#   Yes   658  200

############# Question i ##################
## Evaluate our classifier on validation set
## REMEMBER: Total Error rate = (FalsePos + FalseNeg) / Outcomes

###### logistic regression classifier
lgPrediction = predict(lgfit,newdata = validation, type = "response")
#Transform the predicted probability to a decision and test it
revenuePerProba = vector("numeric",70)
for (p in seq(0.1,0.8,by=0.01)) 
{
  probaStar = p
  ind = 100*p
  print(ind)
  # table to see error rates
  lgDecision = ifelse(lgPrediction > probaStar,1,0)
  classficationTable = table(truth=validation$dog, 
                             predict=lgDecision )
  print(classficationTable)
  revenuePerProba[ind] = sum(classficationTable * revenue)
}
plot(seq(0.1,0.80,by=0.01),revenuePerProba[10:80],pch = 15, xlab = "p*")
## Max Profit = 11.5$ 
## Total Error Rate = 29.9%
# confusion Matrix
#        predict
# truth   0   1
#    No  514  20
#   Yes  227  63

######## Decision tree
pred2 <- predict(tree_datadog, validation, type="class")
table(truth = validation$dog, predict = pred2)
## Max Profit = -4.5$ 
## Total Error Rate = 30.9%
# confusion Matrix
#        predict
# truth   0   1
#    No  490  44
#   Yes  211  79

######## Perfect Classifier
## Max Profit = 290 * 0.5 = 145$
## Total Error Rate = 0%
# confusion Matrix
#        predict
# truth    0    1
#    No  534    0
#   Yes    0  290

# The logistic regression classifier seems to have the lowest total error rate
# and brings the highest profit. We will choose this one.


############# Question j ##################
## to get 1000 pucharses, DogBark has to send on average 20,000 pamphlets to 
## dog owners i.e. predict 'Yes' when it is effectively 'Yes' 20,000 times
## They have to send (100*200+59*200) = 31,800 pamphlets according to the
## confusion matrix

lgPrediction = predict(lgfit,newdata = datadog, type = "response")

#Transform the predicted probability to a decision and test it
revenuePerProba = vector("numeric",70)
for (p in seq(0.1,0.8,by=0.001)) 
{
  probaStar = p
  ind = 100*p
  print(ind)
  # table to see error rates
  lgDecision = ifelse(lgPrediction > probaStar,1,0)
  classficationTable = table(truth=datadog$dog, 
                             predict=lgDecision )
  print(classficationTable)
  revenuePerProba[ind] = sum(classficationTable * revenue)
}
plot(seq(0.1,0.80,by=0.001),revenuePerProba[10:80],pch = 15, xlab = "p*", ylab="Profit")
