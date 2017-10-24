# Open the ibm file

setwd('/Users/NicoTcht/Desktop/ColumbiaUniv/Business_Analytics/Assign2/')
ibmData = read.csv('ibm_return.csv')
ibmData$Date = as.Date(ibmData$Date, "%m/%d/%Y")
summary(ibmData)
names(ibmData)
head(ibmData)

attach(ibmData)

#1) We first divide the data into a training (75%) and test set (25%)
## We notice that we can not pick the data randomly 
## as they are time series
## We thus have to pick them in a chronological order. 
## Indeed, one data depends on what 
## happened in the past.
train_ind=seq(from = 1, to = floor(0.75*nrow(ibmData)), by = 1)
train <- ibmData[train_ind,]
test <- ibmData[-train_ind,]  

# 2) Validation set approach: we divide the training data into four sets of five months.
# In each of this set we have four months for the training and one for the validation
# In the train set it goes from month July 2012 to March 2013 (9 months)

# We prepare our masks
## First set
indtrain1 = Date >= as.Date("2012-07-02") & Date <= as.Date("2012-11-01")
indval1 = Date >= as.Date("2012-11-02") & Date <= as.Date("2012-11-30")
## Second set
indtrain2 = Date >= as.Date("2012-08-01") & Date <= as.Date("2012-11-30")
indval2 = Date >= as.Date("2012-12-01") & Date <= as.Date("2012-12-31")
## Third set
indtrain3 = Date >= as.Date("2012-09-01") & Date <= as.Date("2012-12-31")
indval3 = Date >= as.Date("2013-01-01") & Date <= as.Date("2013-01-30")
## Fourth set
indtrain4 = Date >= as.Date("2012-11-01") & Date <= as.Date("2013-02-28")
indval4 = Date >= as.Date("2013-03-01") & Date <= as.Date("2013-04-01")



indtrain = list(indtrain1,indtrain2,indtrain3,indtrain4)
indval = list(indval1,indval2,indval3,indval4)

# We prepare the 4 sets for the validation 
training=list()
validation=list()
for (i in 1:4){
  training[[i]] = train[indtrain[[i]],][,-1] #we remove 
              #the Data column that we used for the mask
  validation[[i]] = train[indval[[i]],][,-1]
}

#Comments:
## All the training data set are of size 81 (approx)
## All the validation data set are of size 20 (approx)
## Thus we choose a 80% 20% ration between training and validation

# Now let's make a best subset selection to find 
#the best model for each of our four
# training sets
library(leaps) #we load the library to use regsubet function

#Set 1
regfit_list = list()
bestadjR2 = list() #here we are going to store best 
#values of adjr2 for each four sets
for (i in 1:4){
  regfit=regsubsets(as.data.frame(training[i])$Return~., data = as.data.frame(training[i]), nvmax = 8)
  regfit_list[[i]] = regfit
  regfit.summary = summary(regfit)
  names(regfit.summary)
  print(regfit.summary$adjr2)
  bestadjR2[[i]]= which.max(regfit.summary$adjr2)
  
}

# Now we estimate MSE on validation set
val_mse = matrix(rep(NA,32),nrow=4)

for(i in 1:4){       
  val_mat= model.matrix(Return~., data = as.data.frame(validation[i]))
  print("val mat")
  print(val_mat)
  for(j in 1:8){
    coefi=coef(regfit_list[[i]],j)
    pred=val_mat[,names(coefi)]%*%coefi
    print("pred")
    print(pred)
    print("Return")
    print(as.data.frame(validation[i])$Return)
    val_mse[i,j]=mean((as.data.frame(validation[i])$Return-pred)^2)
  }
  }
val_mse

# Subset 1: 1 predictor
# Subset 2: 3 predictors
# Subset 3: 5 predictors
# Subset 4: 2 predictors

t_star=which.min(val_mse)

best_reg= regsubsets(as.data.frame(training[2])$Return~., data = as.data.frame(training[2]), nvmax = 3)

#t_star = 3 this means that the second model is the
#best one (the one which uses 3 predictors)

#train best model on training + validation set
best_reg= regsubsets(train[,-1]$Return~., data = train[,-1], nvmax = 3)
mat_train = model.matrix(Return~., data = train[,-1])
coefi_train= coef(best_reg,3)
pred_train=mat_train[,names(coefi_train)]%*%coefi_train
mse_train=mean((train$Return-pred_train)^2)

## The MSE has a value of 0.97

#evaluate model on test set
mat_test = model.matrix(Return~., data = test[,-1])
coefi_test= coef(best_reg,3)

pred_test=mat_test[,names(coefi_test)]%*%coefi_test
mse_test=mean((test$Return-pred_test)^2)

## The MSE on the test set is 2.0744


# 4) Let's use LASSO regression
library(glmnet)

#Find the best lambda on each of our four test set
lambda <- c(0, .001, .01, .1, 1, 10, 100, 1000)
lambda_star =list()
mse_lasso_best = rep(0,4)

for (i in 1:4){
  mse_lasso = rep(0,8)
  for (j in 1:8){
    ##Solve lasso for each value of λ on training data (50 %)
    x_train <- model.matrix(Return~., training[[i]])[,-1]
    lasso.mod <- glmnet(x_train, training[[i]]$Return, alpha = 1, lambda = lambda[j])
    ##Compute MSE of each model on validation data (25% of data)
    x_valid <- model.matrix(Return~., validation[[i]])[,-1]
    y_valid <- validation[[i]]$Return
    lasso.pred <- predict(lasso.mod, newx = x_valid)
    mse_lasso[j] = mean((lasso.pred[,1]-y_valid)^2)
  }
  lambda_star[i]=which.min(mse_lasso)
  mse_lasso_best[i]=min(mse_lasso)
}

# These are the MSE for each set :
## 1) 0.91108   2) 0.4413   3) 1.4969    4) 0.6285
## On the second set, we have the minimum MSE for Lambda star 
## equal to 0.1
##Thus λ∗=0.1 is the choice of λ corresponding to the 
##smallest MSE on the validation data

#Solve lasso using λ∗ on combined training 
#and validation data (75% of data).
train_75 <- model.matrix(Return~., train[,-1])[,-1]
x_test <- model.matrix(Return~., test[,-1])[,-1]
y_test <- test$Return

lasso.modtest <- glmnet(train_75, train$Return, alpha = 1, lambda = 0.1)
lasso.predtest <- predict(lasso.modtest, newx = x_test)
mse_test = mean((lasso.pred[,1]-y_test)^2)

# The MSE is equal to 2.219 which is better than with the best subset selection method



# 5) We pick the second model (LASSO), the MSE for the validation set was equal to 0.46
# wheres it is equal to 2.219 for the test set



# 6) Create a trading strategy
# Start with $1 of investment and every day 
# select to go either long or short 
# according to the prediction of the mode. What is 
# the return of your trading 
# strategy on the test data?  Based on the results, 
# should you invest using 
# this strategy?

# We are going to pick or LASSO model which had the lowest MSE compare to
# best subset selection
lasso.strategy <- glmnet(train_75, train$Return, alpha = 1, lambda = 0.1)
# Then we are going to predict every return for all 
# the data we did not use
# to create this model (the 25% of the test)
# Indeed, if we were using this model to predict the 
# all data set it woudl be
# meaningless... This would mean that we are trying to 
# forecast the data we used to build our model
# Thus, we are going to consider that we are a trader 
# that can start investing on the
# 2013-04-02 (meaning at the beginning of the test set).

lasso.predstrat <- predict(lasso.strategy, newx = x_test)
Nature_Trade= rep(0,63) ##short or long
Predicted_return = lasso.predstrat
Fund_return = rep(0,63)
Fund_value= rep(1,63)  ## starting with $1

for (i in 1:63){
  if (Predicted_return[i]>=0){
    Nature_Trade[i]="Long"
    if (test$Return[i]>0){
      Fund_return[i]=test$Return[i]
    }else{
      Fund_return[i]=-test$Return[i]
    }
  } else{
    Nature_Trade[i]="Short"
    if (test$Return[i]>0){
      Fund_return[i]=-test$Return[i]
    }else{
      Fund_return[i]=test$Return[i]
    }
  }
  Fund_value[i+1]=Fund_value[i]*(1+Fund_return[[i]]/100)
}
port = ifelse(Nature_Trade == 'Short',-1,1)
Performance = prod(1+port * Fund_return/100)

Nico_fund <- data.frame(test['Date'], Predicted_return, Nature_Trade, Fund_return, Fund_value[-1])
write.fwf(Nico_fund,file="test",sep="\t", quote=F, rownames=T)
# At the end of the year (meaning at the 
# end of the dataset)
# We lose 35% and we definitely should not 
# invest using this strategy...


