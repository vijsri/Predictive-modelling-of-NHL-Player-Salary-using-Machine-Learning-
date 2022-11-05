
# Dealing with Missing Values
-----------------------------

#set current directory
setwd("C:/Users/AB/Desktop/Data 5000 R Code/")

custdata <- read.table("./input/custdata.tsv", header=T, sep="\t")


#for visualization, qplot, ...
library(ggplot2)

#------------------------------
# Spot some problems
# getting dimensions (rows and cols)
# dim returns the dimension (e.g. the number of columns and rows) of a matrix,
# array or data frame.
dim(custdata)

# missing values?
# Lets see how many na we have in each column.
mv <- colSums(is.na(custdata)); mv

# use cbind to display as column
cbind(mv)

#lets see inside of is.employed column
custdata$is.employed

#Recoding missing values
#plyr is a set of tools for a common set of problems: you need to split up a 
#big data structure into homogeneous pieces, apply a function to each piece 
#and then combine all the results back together.
#It is similar to the split and the apply functions in base R.
#For now, we need to load this to be able to use mutate()
#install.packages("plyr")
library(plyr) 


# Missing Categorical Data - Can add a New Category

# We want to convert the missing values fron NA to a new level in a categorical variable.
# Here, ifelse( condition, x, y) works on every elements and if condition is True
# returns x, else returns y. In this case y is True/False i.e. 0 or 1.
# eventually, the output of ifelse is a vector that takes 0, 1, 2 values.
# This vector is converted to a factor with label of the categories are defined by 'labels'
# mutate creates a new column in our dataframe.
custdata <- mutate(custdata,
                   is.employed.fixed = factor(
                     ifelse( is.na(is.employed), 2, is.employed),
                     labels=c("not employed", "employed", "missing")
                   )
)

# to better understand the 'labels' part, lets remove it and see how it looks like without it.
# now the column is.employed.fixed either 0 (false), 1(true) or 2.
custdata <- mutate(custdata,
 is.employed.fixed = factor(
 ifelse( is.na(is.employed), 2, is.employed)
 )
)
cdata <- custdata[c("is.employed","is.employed.fixed")]
head(cdata,10)

# some visualization for age distribution separated by is.employed.fixed categories.
#we use facet_wrap to create separate charts for every category of is.employed.fixed
qplot(age, binwidth=5,
      data=subset(custdata, age>0 & age < 100)) +
  facet_wrap(~is.employed.fixed)

# Relationship with other variables

# Assume if is.employed is NA or income is a negative number then the new variable 
# Income is NA, otherwise it is equal to income.
# mutate adds Income as a new column to custdata.
# now instead of negative values or having positive values for people their 
# is.employed is missing, we have na. So, whatever is in Income column is now my reliable.
custdata <- mutate(custdata,
               Income=ifelse(is.na(is.employed) | income < 0, NA, income))

summary(custdata[c("income", "Income")])

# Missing Continuous Data

#Now lets replace missing values in Income with mean of Income and store the 
# results in a new column Income.fixed
custdata <- mutate(custdata,
              Income.fixed=ifelse(is.na(Income),
                mean(Income, na.rm=TRUE), Income))

# now Income.fix has no missing values.
summary(custdata[c("Income", "Income.fixed")])


#Income may be different in different states.So, lets impute NAs with mean of their state.
# Here the dataframe 'custdata is splitted by 'state.of.res'. Then the function
# mutate is used to create a new column Income.fixed2 which is calculated from Income by replacing
# NAs with mean of Income in their split (which is based on state of residency).
custdata <- ddply(custdata, "state.of.res", mutate,
              Income.fixed2=ifelse(is.na(Income),
                  mean(Income, na.rm=TRUE), Income))

summary(custdata[c("Income", "Income.fixed2")])


#other options: Replace missing Income with 0
custdata <- mutate(custdata,
              Income.fixed3=ifelse(is.na(Income), 0, Income))


# getting average income by state
ddply(custdata, "state.of.res", summarize, 
      avg.income = mean(income, na.rm=TRUE))



# Standardization
# lets z-score age
custdata <- mutate(custdata,
                   age.normalized=(age-mean(age))/sd(age))

summary(custdata[c("age", "age.normalized")])

# Normalize income
custdata <- mutate(custdata,
               income.normalized=income/median(income, na.rm=TRUE) * 100)


#normalize income by state
custdata <- ddply(custdata, .(state.of.res), mutate,
                  income.nrml.by.state=
                    income/median(income, na.rm=TRUE)*100)

summary(custdata[c("income",
                   "income.nrml.by.state")])

# sampling 10%
test.idx <- sample.int(nrow(custdata), 0.1*nrow(custdata))
test.idx
train.idx <- setdiff(1:nrow(custdata), test.idx)
train.idx
train <- custdata[train.idx,]
test <- custdata[test.idx,]


#Here runif generates n=1000 random (uniform distribution) samples between 0 and 1.
custdata <- mutate(custdata, sample=runif(nrow(custdata)))
test <- subset(custdata, sample < 0.1)
train <- subset(custdata, sample >=0.1)


