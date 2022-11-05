# Sample data

install.packages("fortunes")
library(fortunes)
fortune(100)

sentences <- sapply(1:20, function(i) fortune(i)$quote)
df <- data.frame(textCol = sentences)

# The tm (text mining) package
# The documents are provided in a "source" 

install.packages("tm")
library(tm)
df_temp <- data.frame(doc_id=row.names(df), text=df$textCol)
ds <- DataframeSource(df_temp)

# The collection of documents is called "corpus" 
dsc <- Corpus(ds)
inspect(dsc)

# Bag of Words
# We count the frequencies of each word in each document. 
# Before that,we do some processing: remove punctuation, may be also numbers, + also remove so called "stop words". \

dsc.o <- dsc        # save a copy
dsc <- tm_map(dsc, tolower)
dsc <- tm_map(dsc, removePunctuation)
dsc <- tm_map(dsc, removeNumbers)
dsc <- tm_map(dsc, removeWords, stopwords("english"))
inspect(dsc.o[1])
inspect(dsc[1])

# Stemming
# Now we strip the words to their root, so for example "count", "counting", "counted", "counts" all become "count". 
# Stemming algorithms are in package SnowballC 

dsc.p <- dsc        # save another copy
install.packages("SnowballC")
library(SnowballC)
dsc <- tm_map(dsc, stemDocument)
dsc <- tm_map(dsc, stripWhitespace)
inspect(dsc.p[1])
inspect(dsc[1])

# Term Document Matrix
# The Term Document Matrix (TDM) has a row for each term and a column for each document and the value is the count.
# The Document Term Matrix (DTM) is the transpose. 

?TermDocumentMatrix
tdm <- TermDocumentMatrix(dsc)
inspect(tdm)
tdm[["dimnames"]]

# Calculate cosine similarity

library(slam)
cosine_dist_mat <- 1 - crossprod_simple_triplet_matrix(tdm)/(sqrt(col_sums(tdm^2) %*% t(col_sums(tdm^2))))
cosine_dist_mat
