# --------------------------------------------------------------------
# Author: MBNik
# Date: 10th June 2017

# Aim: Discrimination and Classification of the Iris dataset
# --------------------------------------------------------------------

# Libraries:
library(MASS)
library(corrplot)
library(e1071)
library(rpart)

# Clear workspace
rm(list = ls())

# Load data:
myData <- iris
str(myData)

# Data exploration:
M <- cor(myData[,c(1:4)])
corrplot(M, order = 'hclust', addrect = 2)

# Split data in training and test set:
set.seed(10062017)

# -------------------------------
# understanding GLM
# -------------------------------

# This is a step by step explanation who GLM can be applied for a 
# binary classifier using the iris dataset:

# ---------------------------------
# Data manipulation & preparation
# ---------------------------------

# Lets drop the virginica species in order to be able to use
# the iris dataset for a binary classification.
index <- myData$Species=='setosa'
myGLM_Data <- myData[!index,]
myGLM_Data$Species <- factor(myGLM_Data$Species)

# Create some dummy coding 
# => 1 for virginica and 0 for versicolor
myGLM_Data$virginica <- myGLM_Data$Species == 'virginica'

# Split the dataset into training and test dataset:
index <- rownames(myGLM_Data)
TrainIndex <- sample(index,size=70, replace = FALSE)
TestIndex <- which(!index%in%TrainIndex)

myTrainGLM <- myGLM_Data[TrainIndex,]
myTestGLM <- myGLM_Data[TestIndex,]

# -------------------------------
# Create a Logistic Regression Model
# => We would likt to knwo whether the species is setosa or not
# -------------------------------

model1 <- glm(virginica~Sepal.Length+Petal.Length,
             data = myTrainGLM, family = binomial(logit)) 
# Which of the variables are significant?
summary(model1)
# None of the variables seem significant
# => Lets do some data exploration:
M <- cor(myGLM_Data[,c(1:4)])
corrplot(M, order = 'hclust', addrect = 2)
# Quiet some correlation between the variables.

# Lets do a Stepwise Variable Selection
model2 <- step(model1, data = myTrainGLM)

model1 <- glm(virginica ~ Sepal.Width, data = myTrainGLM, family = binomial)
summary(model1)
# Calculate Response
myPrediction <- predict(model1, myTestGLM, type = 'response')

table(Observed=myTestGLM$virginica, Prediction=myPrediction>0.5)

# Create a model 
logit <- glm(setosa~Sepal.Length, data = myGLM_Data, family = 'binomial')
summary(logit)

# Output:
# log(p(X)/1-p(X)) = beta0 + beta1X1 + beta2X2 + beta3X3 + beta4X4
# -64.661 -329.7*Sepal.length+22.465*Petal.Length

Sepal.Length <- myGLM_Data$Sepal.Length
Species <-myGLM_Data$Species
logOdds <- -64.661-(7.140*myGLM_Data$Sepal.Length)+(22.465*myGLM_Data$Petal.Length)
#plot(logOdds)

logOdd2Probabilities <- function(logOdd){
  odds <- exp(logOdd)
  prob <- odds / (1 + odds)
  return(prob)
}

plot(logOdd2Probabilities(logOdds))

myProb_IsVirginia <- logOdd2Probabilities(logOdds)

table(Observed=myGLM_Data$virginica, Prediction=myProb_IsVirginia>0.5)

plot(logit)


table(actual=x$virginica, predicted=pr>.5)

# -------------------------------
fit = glm(vs ~ hp, data=mtcars, family=binomial)
newdat <- data.frame(hp=seq(min(mtcars$hp), max(mtcars$hp),len=100))
newdat$vs = predict(fit, newdata=newdat, type="response")
plot(vs~hp, data=mtcars, col="red4")
lines(vs ~ hp, newdat, col="green4", lwd=2)








