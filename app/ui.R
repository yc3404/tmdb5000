#First step, import the file and clean the data 


#install necessary packages and load libraries
if (!require("tidyverse")) install.packages("tidyverse");library(tidyverse)
if (!require("tm")) install.packages("tm");library(tm)
if (!require("text2vec")) install.packages("text2vec");library(text2vec)
if(!require("shinyWidgets"))install.packages("shinyWidgets");library(shinyWidgets)
if(!require("shiny"))install.packages("shiny");library(shiny)
if(!require("DT"))install.packages("DT");library(DT)
if(!require("data.table"))install.packages("data.table");library(data.table)

#load data
movie_words <- fread("./data/movie_words.csv",encoding="UTF-8",header = T)

#create document matrix and compute tf-idf cosine similarity 
it <- itoken(movie_words$bag_of_words,word_tokenizer)
v <- create_vocabulary(it)
vectorizer <- vocab_vectorizer(v)
dtm <- create_dtm(it,vectorizer)
model_tf <- TfIdf$new()
dtm_tfidf = model_tf$fit_transform(dtm)
cos_sim = sim2(x = dtm_tfidf, method = "cosine", norm = "l2")

#build the function that takes a movie name as input and returns 5 recommended movies
rec <- function(title_input,c = cos_sim){
  recommend_movies <- rep(NA,10)
  #get index that matches the input movie title
  idx <- which(movie_words$title==title_input)
  top_5 <- order(cos_sim[idx,],decreasing=T)[2:11]
  for (i in 1:10){
    recommend_movies[i] <- movie_words[top_5[i],]$title
  }
  return(recommend_movies)
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  includeCSS("style.css"),
  setBackgroundImage(src="https://images.unsplash.com/photo-1478720568477-152d9b164e26?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=crop&w=1950&q=80"),
  fluidRow(
  column(width = 4, div(style = "height:100px;")), 
  column(width = 4, div(style = "height:100px;"), h1("Movie Recommendation", align =
                                                       "center",style="color:white;font-family: Algerian Regular;")), 
         column(width = 4, div(style = "height:100px;"))),
  
  fluidRow(
    column(width = 12, div(style = "height:10px;"))),
  
  fluidRow(
    column(width = 5, div(style = "height:50px;")),
    column(width = 4, div(style = "height:50px;margin:auto;"), 
          selectInput("movie", label=tags$h4("Select your favourite movie",align="center",style="color:white;font-family:Montserrat;"), choices = movie_words$title))),
          
   
fluidRow(column(width=3,offset=5,div(style = "height:60px;"),submitButton(icon("search"), text=tags$b("Find",style="color:white;font-family: Montserrat;")))),
   
fluidRow(style="color:white;
         font-family:Montserrat;font-size:14px;
         ",
         splitLayout(cellWidths=c("20%","20%","20%","20%","20%"),tags$b(textOutput("t1")),
                     tags$b(textOutput("t2")),tags$b(textOutput("t3")),
                     tags$b(textOutput("t4")),tags$b(textOutput("t5"))
                     
         )),
  fluidRow(
    column(width = 12, div(style = "height:40px;"))),
  
    fluidRow(
     splitLayout(cellWidths=c("20%","20%","20%","20%","20%"),imageOutput("mt1"),imageOutput("mt2"),imageOutput("mt3"),
                 imageOutput("mt4"),imageOutput("mt5"))
       
    ),
fluidRow(style="color:white;
         font-family:Montserrat;font-size:14px;
         ",
         splitLayout(cellWidths=c("20%","20%","20%","20%","20%"),tags$b(textOutput("t6")),
                     tags$b(textOutput("t7")),tags$b(textOutput("t8")),
                     tags$b(textOutput("t9")),tags$b(textOutput("t10"))
                     
         )),
   fluidRow(splitLayout(cellWidths=c("20%","20%","20%","20%","20%"),imageOutput("mt6"),imageOutput("mt7"),imageOutput("mt8"),
                          imageOutput("mt9"),imageOutput("mt10")))
)



