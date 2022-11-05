# Other Machine Learning Models

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

plot(tree.A)
text(tree.A, cex=0.7)

# Unsupervised Learning Models
# ----------------------------

# K-Means Clustering
# ------------------

pts <- read.csv("./input/pts_2clusters.csv", header=TRUE)
library(plyr)
pts <- mutate(pts, cl=factor(cl))

library(ggplot2)
qplot(x,y, data=pts)

#plot the true clustering i.e. column cl
qplot(x,y, data=pts, color=cl)

km.out <- kmeans(pts, 2)

# plot the clustering results
qplot(x,y, data=pts, color=factor(km.out$cluster)) 

# Hierarchical Clustering
# -----------------------

library(ggplot2)
library(grDevices)
# hclust(d, method = "complete")
# d is a dissimilarity structure as produced by dist().
#dist() computes  the pairwise distance matrix. 
hc.out <- hclust( dist(pts[c("x","y")]), method="complete")

#plot(hc.out, xlab="", sub="")
plot(hc.out)

# cutree cuts a tree such that it has k leaves
cl.l <-cutree(hc.out, k=5)
qplot(x,y, data=pts, color=factor(cl.l))