# Model Evaluation
# ----------------

# Set up our Logistic Regression Model from last class

setwd('C:/Users/AB/Desktop/Lecture 5/Data 5000 R Code')
Default <- read.csv("./input/Default.csv")

head(Default)
dim(Default)

# Set Outcome Class as Factor
class(Default$default)
Default$default <- as.factor(Default$default)
class(Default$default)

# Split Data Between Test and Training
test <- sample(c(FALSE, TRUE), nrow(Default), replace=TRUE, prob=c(0.8,0.2))
train <- !test
summary(test)
summary(train)

# Create Fitted Model
train.fit <- glm(default ~ ., data=Default, family=binomial,
                 subset=train)
summary(train.fit)

# Run model on Test Data Using Predict()

test.prob <- predict(train.fit, type="response", newdata=subset(Default, test))
head(test.prob)

# Now lets convert probabilities to class labels.
test.pred <- cut(test.prob, breaks=c(0,0.5,1),
                 labels=c("No", "Yes"), include.lowest=TRUE)
head(test.pred)


# So now how do we Evaluate our Model?
# ------------------------------------
  
# Some Naive Evaluation of our Prediction Power
  
testset <- subset(Default, test)
testset$pred.default <- test.pred

head(testset, 200)
summary(testset$default)
summary(testset$pred.default)

# Lets calculate a Confusion Matrix

# One way
lr.ct <- table(Actual=testset$default, Predicted=test.pred)
lr.ct

# Another way - using the Caret library (Classification and Regression)
library(caret)
lr.ct2 <- confusionMatrix(data=test.pred, reference=testset$default)
lr.ct2

# Accuracy of the model is the proportion of cases that were predicted correctly
(lr.ct["Yes", "Yes"] + lr.ct["No", "No"]) / sum(lr.ct)
sum(diag(lr.ct)) / sum(lr.ct)

# Sensitivity is the fraction of actual positives that were correctly classified.
# only 1/3 the default = YES saples are correctly classified. That's bad!
lr.ct["Yes", "Yes"] / sum(lr.ct["Yes", ])

# Specificity is the fraction of actual negatives that were correctly classified.
lr.ct["No", "No"] / sum(lr.ct["No", ])

# Precision is the fraction of cases classified as positives that actually are.
lr.ct["Yes", "Yes"] / sum(lr.ct[, "Yes"])

# Recall is the same as sensitivity
lr.ct["Yes", "Yes"] / sum(lr.ct["Yes", ])



