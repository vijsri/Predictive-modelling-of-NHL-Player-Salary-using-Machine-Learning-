# Some More Advanced Machine Learning Models
# ------------------------------------------

# Use Default Data Set

# K-Nearest Neighbors
# -------------------

# knn(train, test, cl, k = 1)
#install.packages("class")
library(class)
# train is a logical variable.
# Lets split the data into train/test sets.
train.X <- Default[train,c("balance","income")]
test.X <- Default[test,c("balance","income")]
train.Y <- Default$default[train]
test.Y <- Default$default[test]
test.Yknn <- knn(train.X,test.X, train.Y, k=5)
table(Actual=Default$default[test], Predicted=test.Yknn)

# Let's switch Data Sets for the rest of this Lecture

rm(list = ls())
setwd('C:/Users/AB/Desktop/Lecture 5/Data 5000 R Code')

# Read in the Titanic Data Set

titanic <- read.table( "./input/titanic.txt",  sep=",", header=TRUE)
titanic <- titanic[,c("survived", "pclass", "sex", "age")]

# A Little Prep of the data

titanic$age[is.na(titanic$age)] <- mean(titanic$age, na.rm = TRUE)
titanic$survived <- factor(titanic$survived)
titanic$pclass  <- factor(titanic$pclass, order=TRUE, levels = c("1st","2nd","3rd"))
titanic$sex   <- factor(titanic$sex)

# Leave 20% of dataset for testing

test <- sample(c(FALSE, TRUE), nrow(titanic), replace=TRUE, prob=c(0.8,0.2))
train <- !test

# Naive Bayes Example
# -------------------

#install.packages("e1071")
library(e1071)
#install.packages("plyr")
library(plyr)

D <- mutate(titanic, age=cut(age, 10))
summary(D$age)

nb.D <- naiveBayes(survived~pclass + sex + age, data=D, subset=train)

nb.pred <- predict(nb.D, subset(D, test))

test.ct <- table(Actual=D$survived[test], Predicted=nb.pred); test.ct
sum(diag(test.ct)) / sum(test.ct) # Accuracy

# Neural Network Example
# ----------------------

# size indicates number of units in the hidden layer.
# here we have a network with 3 input features, 1 hidden layer with 5 nodes, 
# and one output layer with one node.

#install.packages("nnet")
library(nnet)
nnet.fit <- nnet(survived~pclass + sex + age, data=titanic, subset=train, size=5)

summary(nnet.fit)

nnet.pred <- predict(nnet.fit, newdata=subset(titanic, test), type="class")

test.ct <- table(Actual=titanic$survived[test], Predicted=nnet.pred); test.ct
sum(diag(test.ct)) / sum(test.ct) # Accuracy


# Support Vector Machine (SVM) Example
# ------------------------------------

library(e1071)
svm.fit <- svm(survived~pclass + sex + age, data=titanic, subset=train)
svm.pred <- predict(svm.fit, newdata=subset(titanic, test), type="class")

test.ct <- table(Actual=titanic$survived[test], Predicted=svm.pred); test.ct
sum(diag(test.ct)) / sum(test.ct) # Accuracy

# Classification Trees Example
# ----------------------------

#install.packages("rpart")
library(rpart)

tree.D <- rpart(survived~pclass + sex + age, data=titanic, subset=train, 
                control = rpart.control(minsplit = 2))

D.pred <- predict(tree.D, newdata=subset(titanic,test), type="class")

test.ct <- table(Actual=titanic$survived[test], Predicted=D.pred); test.ct
sum(diag(test.ct)) / sum(test.ct) # Accuracy

tree.D
plot(tree.D)
#Add Text to a Plot
# change font size by changing cex 
#text(tree.D,cex=0.7)
text(tree.D)

# Random Forest Example
# ---------------------

#install.packages("randomForest")
library(randomForest)
RF <- randomForest(survived~pclass + sex + age, data=titanic, subset=train, ntree=100)

RF.pred <- predict(RF, newdata=subset(titanic,test), type="class")
#set type to "prob" to get the probability instead of the class labels
#RF.prob <- predict(RF, newdata=subset(Default,test), type="prob")

test.ct <- table(Actual=titanic$survived[test], Predicted=RF.pred); test.ct
sum(diag(test.ct)) / sum(test.ct) # Accuracy

# Regression Tree Example
# -----------------------

Advertising <- read.csv("./input/Advertising.csv")
head(Advertising)
dim(Advertising)

tree.A <-rpart(sales~TV+radio+newspaper, data=Advertising)
#text(tree.A,cex=0.7)
plot(tree.A)
text(tree.A)
  

