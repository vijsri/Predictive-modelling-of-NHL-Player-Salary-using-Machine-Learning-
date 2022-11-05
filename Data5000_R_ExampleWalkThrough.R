
# Example - Predicting Likelihood of Health Insurance 
#
# This is a base example of working through the data input, data prep, model
# building, model validation, and model evaluation steps that we have discussed
# in the first four lectures (Evaluation will be discussed Lecture 5)
#
# Our fictitious example uses our custdata data set, and attempts to predict
# if a customer has health insurance based on some features we define within
# the rest of the data set.
#
# This is an example of what you would work through for your projects.  You 
# would need to spend more time on identifying features and fitting your model
# than is shown here.  
#
# For R purists, I don't use typical R variable naming conventions - I try to 
# name objects descriptively to help newer R users to work through this code

# Spoiler:  At the end, our model is not great at predicting the negative outcome
# class (no insurance), because the outcomes in our raw data set are not balanced
# If this was a real project, we would definitely want to balance our data set 
# using one of the techniques discussed in Lecture 4.

library(dplyr)
library(ggplot2) # 
library(scales) # Lets us adjust legends/scales in our graphs

# Useful for helping understand the data manipulation library

browseVignettes(package = "dplyr")

# Create Data Frame from File
# ---------------------------

setwd('C:/Users/AB/Desktop/Lecture 4/Data 5000 R Code')
custdata <- read.table("./input/custdata.tsv", header=T, sep="\t")


# Examine Data
# ------------

dim(custdata)
names(custdata)
summary(custdata)

# So what do we see from summary?

   # is.employed, recent.move, num.vehicles contain missing variables (NA's)
   # income has negative values
   # age has a max value of 146.7

# What if we specifically look for missing values

missvalue <- colSums(is.na(custdata))
cbind(missvalue)

  # Notice that housing.type has missing values but this wasn't identified
  # using summary.  Summary does not show NAs for character classes!

# Another way to Examine NAs

complete.cases(custdata)
sum(is.na(custdata$is.employed))
sum(is.na(custdata$recent.move))
sum(is.na(custdata$num.vehicles))

# And another way...

install.packages("Amelia")
library(Amelia)

missmap(custdata, main = "Missing Values")

# Let's start to address these issues in our data
# -----------------------------------------------

# For all missing data - we need to decide what we want to do
# What we do depends on the context of the feature

# Set NA in num.vehicles to 0 (Assumption NA = 0)

install.packages("imputeTS")
library(imputeTS)
custdata$num.vehicles <- na.replace(custdata$num.vehicles,0)
sum((is.na(custdata$num.vehicles)))
custdata$num.vehicles

# Create a new column for is.employed and add a third category (missing)

custdata <- mutate(custdata,
                   is.employed.fixed = factor(
                     ifelse( is.na(is.employed), 2, is.employed),
                     labels=c("not employed", "employed", "missing")
                   )
)

# Check and then get rid of the old is.employed column

dim(custdata)
sum(is.na(custdata$is.employed.fixed))
custdata <- subset(custdata, select = -c(is.employed))
dim(custdata)

# Let's examine the NA's in housing.type and recent.move

sum(is.na(custdata$housing.type)&is.na(custdata$recent.move))
somedata <- select(filter(custdata, is.na(housing.type)), c(housing.type, recent.move))

   # We can see that the NA's are the same between the two columns
   # We will make the decision to remove these 56 observations

custdata <- subset(custdata, !is.na(housing.type))

# Duplicate 56 rows and add to custdata
# Don't do this for your projects - I am just doing it to have
# a clean n=1000 data frame to work with - just for demonstration

somedata2 <- custdata %>% slice(rep(1:56, each = 1))
custdata <- rbind(custdata, somedata2)

# Ok so now we have a data set of n=1000 with no missing values

dim(custdata)
missvalue <- colSums(is.na(custdata))
cbind(missvalue) 

# So now lets deal with outliers

# Lets look at the income feature

summary(custdata$income)
qplot(income, data=custdata, binwidth=10000)

# Clean up the X-axis labels

qplot(income, data=custdata, binwidth=10000)+
   scale_x_continuous(labels=dollar)

# What do we see?  1) We have negative incomes, and
# 2) we have one massive positive outlier (615k)

# For Negative Incomes - lets borrow from the PrepareData script

# Create a new column for income.temp that creates NA for negative incomes

custdata <- mutate(custdata,
                   income.temp=ifelse(is.na(is.employed.fixed) | income < 0, NA, income))

# Now create a column income.fixed that fills NA's with average income
# Note - as per the Visualization script, a better approach would be to 
# update income based on the average income of the state of residency

# Impute values for NA for Income feature...

custdata <- mutate(custdata,
                   income.fixed=ifelse(is.na(income.temp),
                                       mean(income.temp, na.rm=TRUE), income.temp))

summary(custdata[c("income", "income.fixed")])

# For the outlier of $615k, we went back to our interview raw files and found that
# this was a mis-type of $61,500 so lets clean it up

custdata$income.fixed[custdata$income.fixed==615000] <- 61500

# Now lets look at the age feature

summary(custdata$age)
qplot(age, data=custdata)

# Ugh! We have ages above 100 and ages of 0
# As per our lectures, lets normalize age as we are mostly just interested
# relative relationship between the observations, not the raw score

# Transform values for Age feature...

custdata <- mutate(custdata,
                   age.normalized=(age-mean(age))/sd(age))

summary(custdata[c("age", "age.normalized")])
qplot(age.normalized, data=custdata)

# Phew...ok now it looks like we have a clean data set - No missing values,
# No unaccounted for outliers, and all transformations complete


# Create Test and Train Subsets
# -----------------------------

# so we know we are going to try to classify health.ins, so let's make sure
# that this variable is categorical

class(custdata$health.ins)
custdata$health.ins <- as.factor(custdata$health.ins)
class(custdata$health.ins)
summary(custdata$health.ins)  


# Now lets separate our train and test subsets
grouping <- sample(c(rep(0, 0.9 * nrow(custdata)), rep(1, 0.1 * nrow(custdata))))
custdata$grouping <- grouping
table(custdata$grouping)

train.custdata <- custdata[custdata$grouping==0, ]
test.custdata <- custdata[custdata$grouping==1, ]
dim(train.custdata)
dim(test.custdata)

# Develop a Model
# ---------------

# Using our training data we want to start to fit a model
# Given that we want to predict a categorical variable we will
# want to use Logistic Regression (For ML-Classification)

# As a first step lets just throw all features in the model

lr.model <- glm(health.ins ~ ., data=train.custdata, family=binomial)

# We use the Generalized Linear Model function, and identify this as a 
# binomial logistic regression as we only have two outcomes 

# lets look at the results...
summary(lr.model)

# Oh!!! Wow what is going on, we have way too many features and
# a number are not in a form amenable to any sort of analysis
# Let's try again by fitting a more parsimonious model

lr.model <-glm(health.ins~income.fixed, data=train.custdata, family=binomial(link='logit'))
summary(lr.model)

# That's better, it looks like income is statistically significant
# lets add marital status

lr.model <- glm(health.ins~income.fixed + marital.stat, data=train.custdata, family=binomial(link='logit'))
summary(lr.model)

# Hmm...seems like maybe "never married" has a bit of a negative impact on health insurance
# but I think we can do better - lets try adding age

lr.model <- glm(health.ins~income.fixed + age, data=train.custdata, family=binomial(link='logit'))
summary(lr.model)

# Lets focus on the the coefficients to see if they are significant
lr.model$coefficients

# And if you recall we don't have an R^2 for Logit Regression, so we can calculate
# the McFadden R^2 using maximum likelihoods (recall we did this manually
# in the ML1 script from Lecture 4)

install.packages("pscl")
library(pscl)
pR2(lr.model)

# So...for McFadden R^2 index values we want to get closer to .3-.4 to indicate
# a decent fit - so for now our model is not a great fit, so you would have to 
# add some features to the regression to obtain a better fitting model!!

# We can also do an Anova Chi-Squared Goodness of fit assessment
anova(lr.model, test="Chisq")

# yeah - we can see that our model is doing much better than the NULL model
# (NULL model is just the intercept, before adding any features)

# Ok - so we would want to play around and get a better fit overall by
# adding new features, playing with interactions between features, adding 
# higher order features, etc - but for this demo we are going to move on

# Use or Model to Predict Results for Test Data Set
# -------------------------------------------------

# We now want to use predict() to predict results using our test data set
# Recall that we use type="response" to set our output variable as a probability
# rather than the default log-odds ratio

prob.model <- predict(lr.model, type="response")
head(prob.model)

# Lets switch our output to the same factor levels as health.ins (Yes/No)
result.model <- cut(prob.model, breaks=c(0,0.5,1),
               labels=c("FALSE", "TRUE"), include.lowest=TRUE)
head(result.model)
summary(result.model)

# So how did we do?  Evaluate Predictive Power of Model
# -----------------------------------------------------

# For a Naive Approach - lets check fitted values and compare them to the
# actual values

train.custdata$fitvalues <- result.model
compare.model <- subset(train.custdata, select = c("custid", "health.ins", 
         "fitvalues", "income.fixed", "age"))
head(compare.model,50)

# Eye-balling the results we can see that we seem to underestimate
# FALSE values (no Insurance) - this is not surprising!  Why?  Because
# our outcome class is not balanced!  We would need to go back to square
# one and balance our data set using one of the techniques in Lecture 4

summary(compare.model$health.ins)
summary(compare.model$fitvalues)

# Oh boy - we are really underestimating our actual FALSE values but lets
# keep going to at least demonstrate other ways to evaluate our model

# A confusion matrix

# One way
confusion.model <- table(Actual=compare.model$health.ins, Predicted=compare.model$fitvalues)
confusion.model

# Another way - using the Caret library (Classification and Regression)
library(caret)
confusion.model2 <- confusionMatrix(data=compare.model$fitvalues, reference=compare.model$health.ins)
confusion.model2

# So lets calculate accuracy

(confusion.model["TRUE", "TRUE"] + confusion.model["FALSE", "FALSE"]) / sum(confusion.model)
sum(diag(confusion.model)) / sum(confusion.model)

# We have an accuracy of ~.85 which is pretty good, but again keep in mind
# our unbalanced data has a bunch of TRUE's that are matched

# Let's check Specificity - Actual Negatives that were predicted
confusion.model["FALSE","FALSE"] / sum(confusion.model["FALSE", ])

# UGH!  We are only correctly predicting a very low percentage of actual FALSE 
# cases (No Insurance)

# ...Back to the drawing board...