#delete all variables
rm(list = ls())

#get current directory
getwd()

#set current directory
setwd('C:/Users/AB/Desktop/Lecture 4/Data 5000 R Code')

# Sampling Data Sets
# ------------------

somedata <- 1:15
sample(somedata, 2)

custdata <- read.table("./input/custdata.tsv", header=T, sep="\t")

# sampling 10%
test.idx <- sample.int(nrow(custdata), 0.1*nrow(custdata))
test.idx
train.idx <- setdiff(1:nrow(custdata), test.idx)
train.idx
train <- custdata[train.idx,]
test <- custdata[test.idx,]


library(dplyr)
# https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html

#Here runif generates n=1000 random (uniform distribution) samples between 0 and 1.
custdata <- mutate(custdata, sample=runif(nrow(custdata)))
test <- subset(custdata, sample < 0.1)
train <- subset(custdata, sample >=0.1)


# 10 Binomial Trials 
sample(c(0,1), 10, replace = TRUE) 


# Regression Modeling
# -------------------

Advertising <- read.csv("./input/Advertising.csv")

head(Advertising)
names(Advertising)
dim(Advertising)


##Linear Regression
#Use lm to solve linear models
# here output is sales, and input is TV
mod <- lm(sales ~ TV, data=Advertising)
mod
summary(mod)

#Visualize the result
library(ggplot2)
# fitted is returns the fitted values from mod. See mod$fitted
# fitted.values is an alias for fitted
# geom_line() connects the data points (x,y) in order of x.
qplot(TV,sales, data=mod)+
  geom_line(aes(x=TV, y=.fitted), color="blue" )

# geom_linerange draws line ranges, defined by an upper and lower value.
# This is useful e.g., to draw confidence intervals.
# x is the x coordinate of the line.
# ymin is the y coordinate of the lower end of the line
# ymax is the y coordinate of the upper end of the line
last_plot() +
  geom_linerange(aes(x=TV, ymin=.fitted, ymax=sales), color="red")

# Lets predict sales from TV,radio and newspaper input features
mod <- lm(sales~TV+radio+newspaper, data=Advertising)
summary(mod)

# Multicollinearity?
justNews <- lm(sales~newspaper, data=Advertising)
summary(justNews)

noNews <- lm(sales~TV+radio, data=Advertising)
summary(noNews)

## Logistic Regression ------------------------------------
# Lets load a new dataset about credit card defaults.
# FWIW you can download it also using ISLR library.
#install.packages("ISLR")
#library(ISLR)
#data(Default)
Default <- read.csv("./input/Default.csv")

head(Default)
dim(Default)

# Use glm with family=binomial to run a logistic regression.
# The target variable "default" is of type 'character'.
class(Default$default)
# Lets convert it to type 'factor'.
Default$default <- as.factor(Default$default)
class(Default$default)

# Recall that we created Factors from Vectors in Lecture 3

# family=binomial
lr.fit <- glm(default ~ ., data=Default, family=binomial)
summary(lr.fit)

# But where is my R-Squared Goodness-of-Fit?
lr.fit_base <- glm(default~1, data=Default, family="binomial")
1-logLik(lr.fit)/logLik(lr.fit_base)

# Can we graph Logistic Regression?

testfit <-glm(default~balance, data=Default, family=binomial)
summary(testfit)

testfit_base <- data.frame(balance=seq(min(Default$balance), max(Default$balance), len=10000))
testfit_base$default = predict(testfit, testfit_base, type="response")
plot(default~balance, testfit_base, lwd=2)

# Setting Output Variable


# type="link" indicates that the output should be the log-odds ratio.
lr.prob <- predict(lr.fit, type="link")
lr.prob[1:5]

# Our model's response is probabilities
# type="response" indicates that the output should be the probabilities. 
lr.prob <- predict(lr.fit, type="response")
lr.prob[1:5]

# Note that the first 5 samples are all from negative class, so we expect to see
# small probabilities.

# Lets convert the probabilities to predicted values (discrete values)
# cut() divides the range of x (here x is lr.prob) into intervals and codes 
# the values in x according to which interval they fall.
# include.lowest=TRUE indicates that 0 should be included in the first interval.
# note that 1 is by default included in the second interval.
# breaks determines where to break to create intervals.
lr.pred <- cut(lr.prob, breaks=c(0,0.5,1),
               labels=c("No", "Yes"), include.lowest=TRUE)

lr.pred[1:5]
head(lr.pred, 200)

# Model Validation
# ----------------

#confusion matrix -------------------------------
# Lets construct the confusion matrix
# table() creates a frequency table.
# for example, table(Default$default) returns the number of Yes and No values in 
# the default column.
lr.ct <- table(Actual=Default$default, Predicted=lr.pred)
lr.ct

# Accuracy of the model is the proportion of cases that were predicted correctly
(lr.ct["Yes", "Yes"] + lr.ct["No", "No"]) / sum(lr.ct)

# Leave 20% of dataset for testing
# we partition the 10000 samples into two subset, training and testing.
# here we generate a random vector of the length 10000 where each element 
# is either False (with probability of 0.8) or True (with probability of 0.2)
# test is an indicator of which samples belong to the test set.
test <- sample(c(FALSE, TRUE), nrow(Default), replace=TRUE, prob=c(0.8,0.2))

# samples that are not in the test set, will go to the train set.
train <- !test

# subset indicates that only samples with train=True should be used for training.
train.fit <- glm(default ~ ., data=Default, family=binomial,
                 subset=train)

# newdata is the new unseen data, i.e. test data
test.prob <- predict(train.fit, type="response", newdata=subset(Default, test))

# Now lets convert probabilities to class labels.
test.pred <- cut(test.prob, breaks=c(0,0.5,1),
                 labels=c("No", "Yes"), include.lowest=TRUE)

# and finally compute the confusion matrix
test.ct <- table(Actual=Default$default[test], Predicted=test.pred)

test.ct

# Here is accuracy
(test.ct["Yes", "Yes"] + test.ct["No", "No"]) / sum(test.ct)
# we can use diag() to get the diagonal elements of the confusion matrix and calculate accuracy.
sum(diag(test.ct)) / sum(test.ct) # Accuracy


#Notice that the probability of default is low, i.e., the people
#who default are a rare group 333 vs 9667 i.e. only 3% of samples.
tbl <- table(Default$default)
tbl

tbl["Yes"] / sum(tbl)

#If we want to analyze only the people who default, we find
#that our method misses more cases of default than
#identifies! That's bad! 37 errors vs 19 correct predictions inside the class default (i.e. Actual = YES).
test.ct

# Sensitivity is the fraction of actual positives that were correctly classified.
# only 1/3 the default = YES saples are correctly classified. That's bad!
test.ct["Yes", "Yes"] / sum(test.ct["Yes", ])

# Specificity is the fraction of actual negatives that were correctly classified.
test.ct["No", "No"] / sum(test.ct["No", ])

# Precision is the fraction of cases classified as positives that actually are.
test.ct["Yes", "Yes"] / sum(test.ct[, "Yes"])

# Recall is the same as sensitivity
test.ct["Yes", "Yes"] / sum(test.ct["Yes", ])


# KNN-------------------
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



#  Challenge - See if you can get this one working
#  Need to create subsets trainset, testset
#  Need to address missing variable (NA) in original dataset

install.packages("caret")
library(caret)

titanic.url <-
  "https://biostat.app.vumc.org/wiki/pub/Main/DataSets/titanic.txt"
titanic <- read.table( titanic.url,  sep=",", header=TRUE)
titanic
names(titanic)

trainset <- subset(titanic,train) 
testset <- subset(titanic,test)

# define training control
train_control<- trainControl(method="cv", number=10)

# train the model (logistic regression)
model<- train(survived ~., data=trainset, trControl=train_control, method=glm)

# make predictions
predictions<- predict(model,testset)

# append predictions
results<- cbind(testset,predictions)

# summarize results
table(Actual=testset$Survived, results$predictions)

