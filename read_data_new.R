
install.packages("installr")
library(installr)

updateR()
# Download Packages -------------------------------------------------------
install.packages("dplyr")
remove.packages("ggplot2")
install.packages("ggplot2",dependencies = TRUE)
install.packages("colorspace")
install.packages("tidyverse", dependencies = TRUE)
install.packages("superml", dependencies = TRUE)
install.packages("stringr")
install.packages("forcats", dependencies = TRUE)
install.packages("tzdb")
install.packages("SnowballC")
install.packages("caret")



# Read Data ---------------------------------------------------------------

data1 <- read.csv("Train.csv", header=T, nrows=50)
head(data1)

# Data Overview -----------------------------------------------------------

data1 <- as_data_frame(data1)
head(data1)
print(is.data.frame(data1))
#Number of rows:
print(nrow(data1))
#Number of Columns
print(ncol(data1))
#dimensions of Data
dim(data1)
#Glimpse of Data
library(dplyr)
glimpse(data1)
#Summary of Data
summary(data1)

library(ggplot2)
View(data1)


# Finding Duplicates ------------------------------------------------------

library(tidyverse)
duplicated(data1)

data1[duplicated(data1)]


# NEW COLUMN --------------------------------------------------------------
library(stringr)
data1$Count_Tags<- lengths(str_split(data1$Tags," "))


head(data1)
View(data1)



# Analysis of Tags --------------------------------------------------------


library(forcats)
library(dplyr)
library(ggplot2)
library(superml)

cf <- CountVectorizer$new(max_features = 20,remove_stopwords = FALSE,ngram_range = c(1,20))

ans <- cf$fit_transform(data1$Count_Tags)
ans

# #
# titles<-data1 %>%
#   filter(!is.na(data1$Title)) %>%
#   count(data1$Title, sort = TRUE)
# titles

count <- data.frame(table(unlist(strsplit(tolower(data1$Tags),"\ "))))
count<- with(count,count[Var1 != "",])
head(count)

ggplot(count)+geom_bar(aes(x=Var1, y=Freq),fill = 'blue')
#_______________________________--------____________-----__________------____________---------____________---________--------______-
# Number of unique Tags

Tags <- data1 %>%
  filter(!is.na(data1$Tags)) %>%
  count(data1$Tags, sort = TRUE)
print(paste("Number of Data Points:", nrow(data1)))
Tags
print(paste("Number of Data Points:", nrow(Tags)))


#Plot
data1 %>%
  arrange(desc(Tags))%>%
  slice(1:3)%>%
  ggplot(.,aes(Tags))+geom_bar()

ggplot(data1%>% slice_head(n=5), aes(Tags))+ geom_bar()

data1 %>%
  mutate(Tags = Tags %>% fct_infreq()) %>%
  ggplot(aes(Tags)) + geom_bars()

# Plot of number of Tags in each question. 
library(ggplot2)
g<- ggplot(data1,aes(lengths(str_split(Tags," "))))
g + geom_bar()    


# Frequencey of Tags -------------------------------------------------------

output <- data.frame(table(unlist(strsplit(tolower(data1$Tags)," "))))
head(output, n=10)    

class(output)
head(output)
 
ggplot(output, aes(x = Var1, y= Freq)) + geom_bar(stat='identity')



output %>%
  arrange(desc(Freq)) %>%
  slice(1:5) %>%
  ggplot(.,aes(x = Var1, y= Freq)) + geom_bar(stat='identity')


# Distribution of Data ----------------------------------------------------

ggplot(data1,aes(x=(data1),y = Count_Tags))+geom_line(lud =1.25)

# Word cloud --------------------------------------------------------------
install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("wordcloud2")
library(wordcloud2)
install.packages("tm")
library(tm)

text <- data1$Tags

#create corpus
 docs <- Corpus(VectorSource(text))

#Clean Data
docs <- tm_map(docs, content_transformer(tolower))

#create document-term-matrix
dtm <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm)
words <- sort(rowSums(matrix),decreasing = TRUE)
df <- data.frame(word = names(words),freq=words)

#Generate cloud
set.seed(1234)
wordcloud(words=df$word,freq = df$freq, min.freq =1, max.words =200,random.order=FALSE,rot.per=0.35,colors=brewer.pal(8,"Dark2"))

# Preprosesing the Questions in the DATA ----------------------------------

library(tm)
docs <- docs <- Corpus(VectorSource(data1$Title))

library(tm)
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
print(docs)


writeLines(as.character(docs[[1]]))





dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
m

v <- sort(rowSums(m),decreasing=TRUE)
v
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(12)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

corpus <- VCorpus(VectorSource(docs))

# Create a document term matrix.
tdm <- DocumentTermMatrix(corpus, list(removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, removeNumbers = TRUE))
tdm


data1$Tags
# Convert to a data.frame for training and assign a classification (factor) to each document.
train <- as.matrix(tdm)
train
train <- cbind(train,data1$Tags)
train
colnames(train)[ncol(train)] <- 'y'
train <- as.data.frame(train)
train
train$y <- as.factor(train$y)


# Train.
fit <- train(y ~ ., data = train, method = "svmRadial")

# Check accuracy on training.
predict(fit, newdata = train)



# Test data.
data2 <- c("A:dictionary in python and java are same as arrays (take union of dictionaries)?")
corpus <- VCorpus(VectorSource(data2))
tdm <- DocumentTermMatrix(corpus, control = list(dictionary = Terms(tdm), removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, removeNumbers = TRUE))
test <- as.matrix(tdm)
test

# Check accuracy on test.
predict(fit, newdata = test)




#refer to rstudio-pubs-static.s3.amazonaws.com

