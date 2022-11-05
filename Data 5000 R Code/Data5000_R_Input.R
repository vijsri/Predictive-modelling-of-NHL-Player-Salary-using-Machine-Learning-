
# Data Input From Various Sources
# -------------------------------

# File Input

#get current directory
getwd()

#set current directory
setwd("C:/Users/AB/Desktop/Data 5000 R Code/")

# Comma Separated Values (CSV) Files

# Create Data Set from File
x <- read.table('input/mtcars.csv',header = TRUE, sep = ",")
head(x)
tail(x)

mtcars <- read.csv("input/mtcars.csv")
head(mtcars)

#read the first n lines
readLines('input/mtcars.csv', n=3)

# Excel .xlxs files

library('xlsx')
x <- read.xlsx('/input/mtcars.xlsx', sheetIndex = 1)
head(x)

# Console Input 

#Reading in data from the console
#hit the enter key twice which will terminate the scanning.
j <- scan()

#Reading in string data
k <- scan(what=" ")
k[2]

# XML File Input

library('XML')
doc <- xmlParse('input/data.xml')
x <- xmlToDataFrame(doc)
head(x)

# JSON File Input

# Load JSON files
library('rjson')
x <- fromJSON(file = 'input/data.json')
head(x)

# Convert JSON file to a data frame.
x_df <- as.data.frame(x)

# Data Scraping 

# Example 1 - MLB Data

library('httr')
library('XML')
url <- "http://www.baseball-reference.com/leagues/MLB/2021.shtml"
mlb <- readHTMLTable(rawToChar(GET(url)$content))
length(mlb)
names(mlb)
head(mlb$teams_standard_batting)

# Example 2 - Movie Data

library('rvest')
url2 <- "https://www.rottentomatoes.com/top/bestofrt/#top_movies_main"
theWebPage <- read_html(url2)
table_html <- html_table(theWebPage)
mytable <- table_html[[3]]

