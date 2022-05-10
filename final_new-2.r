install.packages("installr")
library(installr)

updateR()
# Download Packages -------------------------------------------------------

library(magrittr)
library(tm)
library(dplyr)
library(plyr)
library(stringr)
library(rvest)
data1 <- read.csv("/Users/asleshach/Murray MSIS/BA Electives/Data Analytics Applications/CIS-663 Project/Questions.csv", header=T, nrows=5000)
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


# remove all special characters in the body and title
# we start with the title
new_df$Title=str_replace_all(new_df$Title, "[^[:alnum:]]", " ")# alpha numeric characters
new_df$Title=str_replace_all(new_df$Title, "[[:punct:]]", " ") # Delete punctuation
new_df$Title=gsub("[[:space:]]", " ", new_df$Title)# all white spaces
# lets go to the Html tags
strip_html <- function(s) {
  html_text(read_html(s))
}

# create a loop to loop over the text in the body colum
new_df$Body=lapply(new_df$Body, strip_html) 
new_df$Body=str_replace_all(new_df$Body, "[^[:alnum:]]", " ")# alpha numeric characters
new_df$Body=str_replace_all(new_df$Body, "[[:punct:]]", " ") # Delete punctuation
new_df$Body=gsub("[[:space:]]", " ", new_df$Body)# all white spaces


# vectorization -----------------------------------------------------------

library(caret)
library(tm)
library(arm)
library(e1071)
library(SnowballC)
library(mldr)
library(superml)
install.packages("randomForest")
library(randomForest)
install.packages("RTextTools")
library(RTextTools)

# we binarize the tags in r

tf_object = TfIdfVectorizer$new(max_df=1, min_df=1, max_features=1, smooth_idf=TRUE)
tf_object$fit(new_df$Tag)
tf_matrix = tf_object$transform(as.vector(new_df$Tag))
final_df=new_df[-3]

# model traing ------------------------------------------------------------
G=create_matrix(new_df$Body, language="english", 
                removeStopwords=FALSE, removeNumbers=TRUE, 
                stemWords=FALSE)

# Training model
K = as.matrix(G)
classifier_RF = randomForest(x = K,
                            y = tf_matrix,
                            ntree = 500)

logistic_model <- glm(tf_matrix~K, 
                      family = "binomial")
logistic_model


 






