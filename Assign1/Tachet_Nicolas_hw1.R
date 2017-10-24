## Assignment 1 - Business Analytics
## Nicolas Tachet - nt2479

#Charge and Explore the data
setwd('/Users/NicoTcht/Desktop/ColumbiaUniv/Business_Analytics/Assign1')
crops=read.csv("Crops_updt.csv")
head(crops)
tail(crops)
summary(crops)
attach(crops)

par(mfrow=c(1,1)) # par(mfrow=c(x,y)) divides plot space into x rows and y colums

#Regress
#2) On water
plot(Water,Yield) 
wat.model = lm(Yield~Water)
wat.model
abline(wat.model)
summary(wat.model)
names(wat.model)

dev.off()

wat.model$residuals
sd(wat.model$residuals)
mean(wat.model$residuals)
boxplot(wat.model$residuals)

#3) On fertilized
plot(Fertilizer,Yield) 
fert.model = lm(Yield~Fertilizer)
fert.model
abline(fert.model)
summary(fert.model)

dev.off()

fert.model$residuals
sd(fert.model$residuals)
mean(fert.model$residuals)
boxplot(fert.model$residuals)

#4) On herbicides
plot(Herbicide,Yield) 
herb.model = lm(Yield~Herbicide)
herb.model
abline(herb.model)
summary(herb.model)
names(herb.model)

dev.off()

herb.model$residuals
sd(herb.model$residuals)
mean(herb.model$residuals)
boxplot(herb.model$residuals)

#5) On all the variables
tot.model = lm(Yield~Herbicide+Fertilizer+Water)
summary(tot.model)

#5) Try to find a conjecture about Fertilizer
boxplot(Yield~Fertilizer)
### We clearly see that for a range of Fertilizer from 5 to 8, 
###the yield is greater.
### We are going to select only the crops with these fertilizer 
### values and consider 
### that the Fertilizer associated is well chosen 
### (appropriateFertilizer = 1)
### Creation of the new dummy variable


crops$appropriateFertilizer=ifelse(crops$Fertilizer >=5 & crops$Fertilizer <= 8 ,1,0)
names(crops)
head(crops)

plot(appropriateFertilizer~Yield)

#Let's make a linear Regression here
naive <- glm(appropriateFertilizer~Yield, family=binomial())
summary(naive)
abline(naive)

#Here we make a Logistic Regression as it should fit the model better
logitReg = glm(appropriateFertilizer~Yield, family=binomial)
summary(logitReg)
curve(predict(logitReg, newdata = data.frame(Yield= x), type = "response"), add=TRUE)

crops2 <- data.frame(Yield, Water, appropriateFertilizer, Herbicide)

#8) Run a regression with an interaction
interact.model = lm(crops$Yield~crops$Water*crops$appropriateFertilizer,data=crops2)
summary(interact.model)
### It seems that the variable appropriateFertilizer does not explain 
### anything (small p-value)
### However, there is a strong correlation between this variable and 
### Water that is positive.
### This explains why our F-Statistic for the model is greater than 
### the previous one. The model is significantly better.

#9) Let's try it without the appropriateFertilizer variable 
### (just the interaction and water).
fitting = lm(Yield ~ Water + Water:appropriateFertilizer, data=crops2)
summary(fitting)
### We see that the F-Statistic is 3007 which is far more greater than for
### the previous regression.
### We improved the model by removing the appropriateFertilizer variable.

#10) Confidence interval 99%
confint(fitting,level=0.99)
### Thus, there is 99% probability that the calculated confidence interval 
### from some future experiment 
### encompasses the true value of the parameters we are trying to evaluate.

#11) prediction
testdata = data.frame(Water=30, appropriateFertilizer=1, Herbicide=5)
predict(fitting, testdata, interval="prediction", level=0.9)

