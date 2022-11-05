
# Data Visualization (Exploration)
# --------------------------------

#get current directory
getwd()

#set current directory
setwd("C:/Users/AB/Desktop/Data 5000 R Code/")

#install graphing package
library(ggplot2)

mtcars <- read.table('./input/mtcars.csv',header = TRUE, sep = ",")

# Basic qplot

qplot(mpg, disp, data=mtcars)

qplot(mpg, disp, data=mtcars, colour= factor(cyl),
      main="Engine displacement vs MPG", xlab="MPG",
      ylab="Engine displacement (cb.in)")

# Adding Themes

# theme_bw()  - a theme with a white background
qplot(mpg, disp, data=mtcars, colour=factor(cyl),
      main="Engine displacement vs MPG", xlab="MPG",
      ylab="Engine displacement (cb.in)") +
  theme_bw() + labs(colour="Cylinders")

# another themes: theme_classic, theme_minimal, theme_grey
qplot(mpg, disp, data=mtcars, colour=factor(cyl),
      main="Engine displacement vs MPG", xlab="MPG",
      ylab="Engine displacement (cb.in)") +
  theme_minimal() + labs(colour="Cylinders")

# Using ggplot and trendlines

ggplot(mtcars, aes(x=mpg, y=disp)) +
  geom_point() +
  geom_smooth(method=lm) #add linear trend line


# Using Data Visualization to Explore Data 
# ----------------------------------------

# Customers with Health Insurance Data (tab delimited)
custdata <- read.table("./input/custdata.tsv", header=T, sep="\t")

# dim returns the dimension (e.g. the number of columns and rows) of a matrix, array or data frame.
#colnames returns the name of columns
dim(custdata)
colnames(custdata)

#use [[ to access to a single element by index or name
#access element at row 3 and column 4
custdata[[3,4]]

#use [ to access to multiple elements by index or name
#access the first two rows, only the first 4 columns
custdata[1:2,1:4]
#access the first two rows, only sex and income columns
custdata[1:2,c('sex','income')]

#use $ to access to a single element by name
custdata$income

#tip: use ctrl+L to clear the console.

# Cross Tab Analysis

#Lets create a new table with two columns: sex and health.ins
tab <- table(custdata$sex,custdata$health.ins)
tab

# Adding row totals
#We want to add a new raw "total" which shows total sum of every column.
#use rowSums or colSums to take sum over rows or columns respectively. 
#then use cbin to append the new column to the existing columns.
tab <- cbind(tab, Total = rowSums(tab)); tab

# Adding column totals
tab <- rbind(tab, Total = colSums(tab)); tab

# Exploring Data

#Lets find values out of range
summary(custdata$income)
summary(custdata$age)

# Out of range values can be caught easier by visualizing values
# here is a bar chart with 30 bins 
qplot(age, data=custdata, bins = 30)

#Try it again by setting binwidth rather than number of bins.
qplot(age, data=custdata, binwidth = 10)

#Now lets plot 'income' 
qplot(income, data=custdata, binwidth=5000)

# Lets make the x axis prettier by  using dollar format. 
#install.packages(scales)
library(scales)
qplot(income, data=custdata, binwidth=5000)+
 scale_x_continuous(labels=dollar)

#the same plot can be done using the following commands
ggplot(custdata) +
  geom_histogram(aes(x=income), binwidth=5000) +
  scale_x_continuous(labels=dollar)


# Continuous Variables

#Logarithmic Scale plots
custdata2 <- subset(custdata, income > 0)

qplot(income, data=custdata2, binwidth=5000) +
  scale_x_log10(breaks=10^(1:6), labels=dollar)

# Note, since we are using logaritmic scale, the binwidth should
# be in percent change, not dollar amount!
# binwidth=0.1 means we will have 10 bins per decade. 
# 'breaks' indicates where to put x axis ticks and dollar values. 
qplot(income, data=custdata2, binwidth=0.25) +
  scale_x_log10(breaks=10^(1:6), labels=dollar)

#Density Plots
# 'geom' defines the type of plot as 'density'.
# bw=0.2 to change the std of the kernel used to estimate the density
# role of bw is similar to binwidth. Both control how local data are treated.
qplot(income, data=custdata2, geom="density") +
  scale_x_log10(breaks=10^(1:6), labels=dollar)

# Categorical Variables

#Bar Charts
# Now that you have seen examples of visualization of continues variables,
# lets see what we can do with categorical variables.
# A bar chart is a histogram for categorical variable. It is the
# default geometry in qplot for factor and logical variables.
qplot(marital.stat, data=custdata)

# try 'state of residence' variable which has more categories. 
qplot(state.of.res, data=custdata)
# x axis ticks doesn't look good.

#Lets try a horizental bar chart
qplot(state.of.res, data=custdata) + coord_flip()

#We can make it even better by using smaller font size, to avoid overlaps.
#Themes are a powerful way to customize the non-data components of your 
# plots: i.e. titles, labels, fonts, background, gridlines, and legends.
qplot(state.of.res, data=custdata) + coord_flip() +
  theme(axis.text.y=element_text(size=rel(0.5)))

# Reorder the Data

custdata <- transform(custdata, state.of.res.ord=
                        reorder(state.of.res, state.of.res, length))

qplot(state.of.res.ord, data=custdata) + coord_flip() +
  theme(axis.text.y=element_text(size=rel(0.8)))



#split the custdata' into two groups based on sex variable.
#split/apply/combine
pieces <- split(custdata, custdata$sex)

result <- lapply(pieces, function(p)
  data.frame(
    sex=p$sex[[1]],
    sex.avg.vehicles=mean(p$num.vehicles, na.rm=TRUE)
  )
)
# combine
result <- do.call("rbind", result)
result


#split the custdata' into 52 groups based on state.of.res variable.
pieces <- split(custdata, custdata$state.of.res)
# apply
result <- lapply(pieces, function(p) 
  data.frame(
  state.of.res=p$state.of.res[[1]],
  state.avg.vehicles=mean(p$num.vehicles, na.rm=TRUE)
            )
)
# combine
result2 <- do.call("rbind", result)

a <- rbind(result[[1]],result[[2]],result[[3]])


#Package plyr implements split-apply-combine framework very
#neatly in a single function call.
#summarize creates a new data.frame from an old one.
library(plyr)
result <- ddply(
  custdata,            # dataframe
  "state.of.res",      # split-by variables
  summarize,           # function to apply to each piece
                       # function arguments
  state.avg.vehicles=mean(num.vehicles, na.rm=TRUE)
)

#summarize(custdata,state.avg.vehicles=mean(num.vehicles, na.rm=TRUE),b=mean(income))

# Now lets plot one variable vs another
# Scatter Plot
custdata2 <- subset(custdata, age>0 & age < 100 & income > 0)
qplot(age, income, data=custdata2) +
  scale_y_continuous(labels=dollar)

# now lets put some color on each point
qplot(age, income, data=custdata2, colour=health.ins) +
  scale_y_continuous(labels=dollar)

# still many of points overlap.
#lets make it a 2D histogram.
qplot(age, income, data=custdata2, geom="bin2d") +
  scale_y_continuous(labels=dollar)

# instead of squar bins, we can have hex bins.
#install.packages('hexbin')
library(hexbin)
qplot(age, income, data=custdata2, geom="hex") +
  scale_y_continuous(labels=dollar)

#a scatter plot also works for continuous vs. categorical. 
qplot(age, health.ins, data=custdata2)

#This is better - it gives a better feel for the density at each level.
qplot(age, health.ins, data=custdata2,
      geom="jitter", height=0.3)

# color the jittered points according to income.
# to make the colormap nicer, we use log of income. 
# so the colors better reflect the ratio rather than income differences.
qplot(age, health.ins, data=custdata2, color=log10(income),
      geom="jitter", height=0.2)

#Bar Chart for Two Variables
#Use the fill aesthetic as the second variable.
# this is bar chart of marital status. Each bin is divided 
# according to whether they have health insurance or not.
qplot(marital.stat, data=custdata2, fill=health.ins)

#here is another example. This is histogram of housing type. 
#However each bin is divided according to the marital status which has four categories. 
#lets rotate the x axis text to avoid overlapping texts.
qplot(housing.type, data=custdata2, fill=marital.stat)+
 theme(axis.text.x=element_text(angle=15))

qplot(housing.type, data=custdata2, fill=marital.stat)+
  theme(axis.text.x=element_text(angle=15))

#subset(custdata2, !is.na(housing.type)))

# Lets use subset to remove na values.
qplot(housing.type, data=subset(custdata2, !is.na(housing.type)), fill=marital.stat)+
  theme(axis.text.x=element_text(angle=15))

