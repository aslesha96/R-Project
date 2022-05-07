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
library(magrittr)


library(tm)
library(dplyr)
library(plyr)
data1<- read.csv("/Users/asleshach/Murray MSIS/BA Electives/Data Analytics Applications/CIS-663 Project/Train.csv",header = T, nrows = 5000)
head(data1)

data2=read.csv("/Users/asleshach/Murray MSIS/BA Electives/Data Analytics Applications/CIS-663 Project/Tags.csv", header=T, nrows=5000)
tags=ddply(data2, .(Id), summarise, Tag=paste0(Tag, collapse=", "))

# we drop some rows in the data
data1=data1[,-c(2,3,4)]

# lets merge the  two datasets
jointdataset <- merge(data1,tags, by = 'Id')

# now we take the questions that have a score of above 5
new_df=jointdataset[jointdataset$Score>5,]
# we do not need score and id
new_df=new_df[,-c(1,2)]


# data cleaning on the body

new_df$Body <- iconv(new_df$Body)
new_df$Body <- Corpus(VectorSource(new_df$Body))
#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
new_df$Body<- tm_map(new_df$Body, toSpace, "/")
new_df$Body <- tm_map(new_df$Body, toSpace, "@")
new_df$Body <- tm_map(new_df$Body, toSpace, "\\|")
# Convert the text to lower case
new_df$Body <- tm_map(new_df$Body, content_transformer(tolower))
# Remove numbers
new_df$Body <- tm_map(new_df$Body, removeNumbers)
# Remove english common stopwords
new_df$Body <- tm_map(new_df$Body, removeWords, stopwords("english"))
# Remove your own stop word
# specify your custom stopwords as a character vector
new_df$Body <- tm_map(new_df$Body, removeWords, c("s", "company", "team")) 
# Remove punctuations
new_df$Body<- tm_map(new_df$Body, removePunctuation)
# Eliminate extra white spaces
new_df$Body <- tm_map(new_df$Body, stripWhitespace)
# Text stemming - which reduces words to their root form
new_df$Body <- tm_map(new_df$Body, stemDocument)



# we repeat the process for the title

# data cleaning on the body

new_df$Title <- iconv(new_df$Title)
new_df$Title <- Corpus(VectorSource(new_df$Title))
#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
new_df$Title<- tm_map(new_df$Title, toSpace, "/")
new_df$Title <- tm_map(new_df$Title, toSpace, "@")
new_df$Title <- tm_map(new_df$Title, toSpace, "\\|")
# Convert the text to lower case
new_df$Title <- tm_map(new_df$Title, content_transformer(tolower))
# Remove numbers
new_df$Title <- tm_map(new_df$Title, removeNumbers)
# Remove english common stopwords
new_df$Title <- tm_map(new_df$Title, removeWords, stopwords("english"))
# Remove your own stop word
# specify your custom stopwords as a character vector
new_df$Title <- tm_map(new_df$Title, removeWords, c("s", "company", "team")) 
# Remove punctuations
new_df$Title<- tm_map(new_df$Title, removePunctuation)
# Eliminate extra white spaces
new_df$Title <- tm_map(new_df$Title, stripWhitespace)
# Text stemming - which reduces words to their root form
new_df$Title <- tm_map(new_df$Title, stemDocument)


Y=mldr_transform(new_df$tags, type = "BR", labels)
## 75% of the sample size
smp_size <- floor(0.80 * nrow(new_df))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(new_df)), size = smp_size)

train <- new_df[train_ind, ]
test <- new_df[-train_ind, ]
# vectorization -----------------------------------------------------------
install.packages("caret")
install.packages("tm")
install.packages("arm")
install.packages("e1071")
install.packages("SnowballC")

library(caret)
library(tm)
library(arm)
library(e1071)
library(SnowballC)
library(mldr)
library(superml)


# we binarize the tags in r
final_df=new_df[-3]
tf_object = TfIdfVectorizer$new(max_df=1, min_df=1, max_features=1, smooth_idf=TRUE)
tf_matrix = tf_object$transform(final_df)






# model traing ------------------------------------------------------------

# Training model
classifier_RF = randomForest(x = tf_matrix,
                            y = new_df$tags,
                            ntree = 500)

classifier_RF

# Predicting the Test set results
y_pred = predict(classifier_RF, newdata = test[-1])


summary(logistic_model)


# one-vs-rest -------------------------------------------------------------

n <- ncol(train)
set.seed(1234)
training_samples <- sample(1:n,size = n*0.6)

train <- data1[1:1000,training_samples]
test  <- data1[1:1000,-training_samples]

# just to be sure there are no shared samples between the training and testing data
sum(sampleNames(test) %in% sampleNames(train)) == 0



object <- readData(Data = train, 
                   Labels = "Data1", 
                   Platform = NULL, 
                   verbose = FALSE)
object

#-------------------------------------------------------------------------
bio$injury <- data1
nfolds <- round(281*0.75)
nfolds




