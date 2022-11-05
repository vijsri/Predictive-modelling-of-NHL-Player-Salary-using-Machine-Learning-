
# Install R from http://www.r-project.org
# Install RStudio from https://www.rstudio.com

# Useful Guide:  https://www.rdocumentation.org/
# Use # for comments

# Use ? for Help
?data
?help

# Some Basic Functionality
# ------------------------

2+2
x=5
y<-7
3 < 2 || 1 > 2
round(pi, digits = 4)
sum(1:5)
mystring = "Data 5000"
nchar(astr)
length(astr)
strsplit(mystring, ' ')


# Vectors and Matrices
# --------------------

w = c(1, 2, 3, 4, 5, 6)
v = c(2, 3, 4, 5, 6, 7)
z <- w+v
m = matrix(z, nrow=3, ncol=2)
t(m)
m*2
m %*% t(m)


# Categorical Data (Factors)
# --------------------------

# Nominal 

season_vector <- c("Spring", "Summer", "Fall", "Winter")
season_fact <- factor(seasons)
season_fact

# Ordinal

cost_vector <- c('-10', '10-20', '20+')
cost_fact <- factor(cost_vector, order = TRUE, levels = c('-10', '10-20', '20+'))
cost_fact

# Data Frames
# -----------

cars
carlist <- head(cars,5)
dim(cars)
names(cars)
colMeans(cars)

# Libraries / Packages
# --------------------

?library

someString <-  '   I have spaces   '
someString
nchar(someString)

# Remove leading and trailing zeros
str_trim(someString)
library(stringr)
someString <- str_trim(someString)
nchar(someString)
