#install.packages("syuzhet")
#install.packages("tm")
#install.packages("SnowballC")
#install.packages("RColorBrewer")
library("syuzhet")
library("ggplot2")
library("tm")
library("SnowballC")
library("RColorBrewer")
library("keras")
dataset = readLines(file.choose())
TextDoc = Corpus(VectorSource(dataset))
#Replacing "/", "@" and "|" with space
toSpace = content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc = tm_map(TextDoc, toSpace, "/")
TextDoc = tm_map(TextDoc, toSpace, "@")
TextDoc = tm_map(TextDoc, toSpace, "\\|")
# Convert the text to lower case
TextDoc = tm_map(TextDoc, content_transformer(tolower))
# Remove numbers
TextDoc = tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
TextDoc = tm_map(TextDoc, removeWords, stopwords("english"))
# Remove your own stop word
# specify your custom stopwords as a character vector
TextDoc = tm_map(TextDoc, removeWords, c("s", "company", "team")) 
# Remove punctuations
TextDoc = tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc = tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
TextDoc = tm_map(TextDoc, stemDocument)
# Build a term-document matrix
TextDoc_dtm = TermDocumentMatrix(TextDoc)
dtm_m = as.matrix(TextDoc_dtm)
# Sort by descearing value of frequency
dtm_v = sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d = data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 5)
# Plot the most frequent words
barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,col ="lightgreen", main ="Top 5 most frequent words",ylab = "Word frequencies")
# regular sentiment score using get_sentiment() function and method of your choice
# please note that different methods may have different scales
syuzhet_vector = get_sentiment(dataset, method="syuzhet")
# see the first row of the vector
head(syuzhet_vector)
# see summary statistics of the vector
summary(syuzhet_vector)
# bing
bing_vector = get_sentiment(dataset, method="bing")
head(bing_vector)
summary(bing_vector)
#affin
afinn_vector = get_sentiment(dataset, method="afinn")
head(afinn_vector)
summary(afinn_vector)
#compare the first row of each vector using sign function
rbind(sign(head(syuzhet_vector)),sign(head(bing_vector)),sign(head(afinn_vector)))
# run nrc sentiment analysis to return data frame with each row classified as one of the following
# emotions, rather than a score: 
# anger, anticipation, disgust, fear, joy, sadness, surprise, trust 
# It also counts the number of positive and negative emotions found in each row
d = get_nrc_sentiment(dataset)
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head(d,10)
#transpose
td = data.frame(t(d))
#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new = data.frame(rowSums(td[2:11]))
#Transformation and cleaning
names(td_new)[1] = "count"
td_new = cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) = NULL
td_new2 = td_new[1:8,]
#Plot One - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")
#Plot two - count of words associated with each sentiment, expressed as a percentage
barplot(sort(colSums(prop.table(d[, 1:8]))), horiz = TRUE, cex.names = 0.7, las = 1, main = "Emotions in Text", xlab="Percentage")
