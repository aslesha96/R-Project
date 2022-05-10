#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(magrittr)
library(tm)
library(dplyr)
library(plyr)
library(stringr)
library(rvest)
library(caret)
library(tm)
library(arm)
library(e1071)
library(SnowballC)
library(mldr)
library(superml)
library(randomForest)
library(RTextTools)

data1 <- read.csv("C:\\Users\\ad\\Downloads\\ques\\Questions.csv", header=T, nrows=5000)
head(data1)

data2=read.csv("C:\\Users\\ad\\Downloads\\Tags\\Tags.csv", header=T, nrows=5000)
tags=ddply(data2, .(Id), summarise, Tag=paste0(Tag, collapse=", "))


ui <- basicPage(
  sidebarPanel(
    pickerInput("locInput","Select a regression model to use", choices=c("Random forest", "logistic regression"), 
                options = list(`actions-box` = TRUE),multiple = F, mainPanel(
      verbatimTextOutput('structure'))
  )
)
)

server <- function(input, output) {
  
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
  # lets go to the Html tags
  strip_html <- function(s) {
    html_text(read_html(s))
  }
  
  # create a loop to loop over the text in the body colum
  new_df$Body=lapply(new_df$Body, strip_html) 
  new_df$Body=str_replace_all(new_df$Body, "[^[:alnum:]]", " ")# alpha numeric characters
  new_df$Body=str_replace_all(new_df$Body, "[[:punct:]]", " ") # Delete punctuation
  new_df$Body=gsub("[[:space:]]", " ", new_df$Body)# all white spaces
  
  tf_object = TfIdfVectorizer$new(max_df=1, min_df=1, max_features=1, smooth_idf=TRUE)
  tf_object$fit(new_df$Tag)
  tf_matrix = tf_object$transform(as.vector(new_df$Tag))
  final_df=new_df[-3]
  
  G=create_matrix(new_df$Body, language="english", 
                  removeStopwords=FALSE, removeNumbers=TRUE, 
                  stemWords=FALSE)
  classifier_RF = randomForest(x = K,
                               y = tf_matrix,
                               ntree = 500)
  logistic_model <- glm(tf_matrix~K, 
                        family = "binomial")
  
  # Training model
  K = as.matrix(G)
  
  output$structure <-
    renderTable({
      if (input$locInput=='Random forest'){
        summary(classifier_RF)}
        else {
          summary(logistic_model)
        }
      
      })
  
}

shinyApp (ui = ui, server = server)