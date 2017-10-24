# Assignment 3 - Nicolas Tachet - nt2479
## CUISINE_PREF ##

setwd('/Users/NicoTcht/Desktop/ColumbiaUniv/Business_Analytics/Assign3')
cuisine = read.csv('cuisinePreferences.csv')
summary(cuisine)

#########################################
######### Question A ####################
# We select section 1 as the training set
train <- subset(cuisine, Section == 1)
test <- subset(cuisine, Section ==2)

#########################################
######### Question B ####################

# We create our dataframe of preference
myPref = data.frame(X = 'Nicolas Tachet', Italian = 5,
                   Mexican = 4, Chinese...Cantonese = 1, Chinese....Sichuan = 1, 
                   Greek = 3, Thai = 5, Indian = 4, French = 5, Steakhouse = 4,
                   Ethiopian = 2, Spanish = 4, Carribean = 2, Seafood= 5, Vegan = 1,
                   Sushi = 5, Pub.food=2, Vietnamese = 4, Middle.Eastern = 2
                   )


outcomeColNum = which(colnames(train) == "Section")
train <- train[,-outcomeColNum]

# We define a function calculating the euclidean distance beteen two vectors
euclidedist = function(x,y){
  n = length(x)
  distance = 0
  compteur = 0
  for (i in 1:n){
      if (!is.na(x[i]) & !is.na(y[i]) ){
          distance = distance + (as.double(x[i])-as.double(y[i]))^2
          compteur = compteur + 1
      }
      }
  distance = sqrt(distance/compteur) 
  return(distance)
}

train_me = rbind(myPref, train)

#We create the matrix with the distance between each students
A= matrix('double', 3969, nrow=63,ncol=63) # 62 students + me
for (i in 1:63){
  for (j in 1:63){
    A[i,j]=euclidedist(train_me[i,],train_me[j,])
  }
}
A = mapply(A, FUN=as.double)
A = matrix(A, ncol = 63, nrow = 63)
myNeighbors = A[,63][-63]
my5nearestNeighbors = order(myNeighbors)[1:5]
# My 5 nearest neighbors are
#36: Lars-Patrik Roeller
#6: Ling Dong
#27: Kevin Qiu
#41: Mathieu Nohet
#15: Zhi Li

#########################################
######### Question C ####################

# first column is the names, make it the row labels instead
cuisineNames = colnames(train_me)
rownames(train_me) = train_me[,1]
# remove for row and convert to matrix format
# (transforming to a matrix helps speed up some of the calculations below)
cuisine = as.matrix(train_me[,-1])

# n = number of students
n = nrow(cuisine)
# m = number of cuisines
m = ncol(cuisine)

predictRatings<- function(nn, cuisine){
  
predictionMatrix = data.frame()

for (u in 1:n)
{
  tieBreak = c(u,1:n)[!duplicated(c(u,1:n))]
  orderedCuisine = cuisine[order(A[u,],tieBreak),]
  for (cui in 1:m)
  {
    ratings = na.omit(orderedCuisine[2:n,cui])
    ratings = as.vector(ratings)
    if (length(ratings) > nn )
      predictionMatrix[u,cui] = mean(ratings[1:nn])
    else
      predictionMatrix[u,cui] = mean(ratings)
  }
}
colnames(predictionMatrix) = colnames(cuisine)
row.names(predictionMatrix) = row.names(cuisine)
return(predictionMatrix) 
}

## Our answer
ANSWER_3NN = predictRatings(3,train_me)

#########################################
######### Question D ####################

# Test for best nn value
#####################

# this vector holds the RMSE for each value of nn in 1:10
RMSEperformance = vector(length = 20)

# calculate the RSME for each value of nn
for ( nn in 1:20){
  predictionMatrix = predictRatings(nn,cuisine)
  RMSEperformance[nn] = sqrt(mean((predictionMatrix - cuisine)^2,na.rm = TRUE))
}

# plot the result
plot(1:20, RMSEperformance, xlab = "Number of nearest neighbors")

## Best for NN=4 with RMSE = 1.033670

#########################################
######### Question E ####################

outcomeColNum = which(colnames(test) == "Section")
test <- test[,-outcomeColNum]

A= matrix('double', 3364, nrow=58,ncol=58) # 62 students + me
for (i in 1:58){
  for (j in 1:58){
    A[i,j]=euclidedist(test[i,],test[j,])
  }
}
A = mapply(A, FUN=as.double)
A = matrix(A, ncol = 58, nrow = 58)

cuisineNames = colnames(test)
rownames(test) = test[,1]
# remove for row and convert to matrix format
# (transforming to a matrix helps speed up some of the calculations below)
cuisine = as.matrix(test[,-1])

# n = number of students
n = nrow(cuisine)
# m = number of cuisines
m = ncol(cuisine)

Prediction_Section2 = predictRatings(4, cuisine)
# We want to compute the RMSE for three students
# Let's say we consider Pierre Laurent, Christina Papadimitriou and Omar Abboud
Pred = Prediction_Section2[c('Pierre Laurent','Christina Papadimitriou','Omar Abboud'),]
Testo = test[c('Pierre Laurent','Christina Papadimitriou','Omar Abboud'),]

# remove for row and convert to matrix format
outcomeColNum = which(colnames(Testo) == "X")
Testo <- Testo[,-outcomeColNum]
 
RMSEperformance_Final = sqrt(mean((Pred - Testo)^2,na.rm = TRUE))

