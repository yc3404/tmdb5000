
#install necessary packages and load libraries
if (!require("tidyverse")) install.packages("tidyverse");library(tidyverse)
if (!require("tm")) install.packages("tm");library(tm)
if (!require("text2vec")) install.packages("text2vec");library(text2vec)
if (!require("shiny")) install.packages("shiny");library(shiny)
if (!require("DT")) install.packages("DT");library(DT)
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
#recommend function that recommends 10 movies
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

shinyServer(function(input, output) {
  
  output$mt1 <- renderImage({
    m_i<- str_replace_all(rec(input$movie),"[[:punct:]] | [^[:ascii:]]"," ")
    filename <- normalizePath(file.path(
      paste("./www/img/",m_i[1], ".JPEG", sep = "")))
    
    list(src=filename)
  },deleteFile = FALSE)
  
  output$mt2 <- renderImage({
    m_i<- str_replace_all(rec(input$movie),"[[:punct:]] | [^[:ascii:]]"," ")
    filename <- normalizePath(file.path(
      paste("./www/img/",m_i[2], ".JPEG", sep = "")))
    
    list(src=filename)
  },deleteFile = FALSE)
  
  output$mt3 <- renderImage({
    m_i<- str_replace_all(rec(input$movie),"[[:punct:]] | [^[:ascii:]]"," ")
    filename <- normalizePath(file.path(
      paste("./www/img/",m_i[3], ".JPEG", sep = "")))
    
    list(src=filename)
  },deleteFile = FALSE)
  
  output$mt4 <- renderImage({
    m_i<- str_replace_all(rec(input$movie),"[[:punct:]] | [^[:ascii:]]"," ")
    filename <- normalizePath(file.path(
      paste("./www/img/",m_i[4], ".JPEG", sep = "")))
    
    list(src=filename)
  },deleteFile = FALSE)
  
  output$mt5 <- renderImage({
    m_i<- str_replace_all(rec(input$movie),"[[:punct:]] | [^[:ascii:]]"," ")
    filename <- normalizePath(file.path(
      paste("./www/img/",m_i[5], ".JPEG", sep = "")))
    
    list(src=filename)
  },deleteFile = FALSE)
  
  output$mt6 <- renderImage({
    m_i<- str_replace_all(rec(input$movie),"[[:punct:]] | [^[:ascii:]]"," ")
    filename <- normalizePath(file.path(
      paste("./www/img/",m_i[6], ".JPEG", sep = "")))
    
    list(src=filename)
  },deleteFile = FALSE)
  
  output$mt7 <- renderImage({
    m_i<- str_replace_all(rec(input$movie),"[[:punct:]] | [^[:ascii:]]"," ")
    filename <- normalizePath(file.path(
      paste("./www/img/",m_i[7], ".JPEG", sep = "")))
    
    list(src=filename)
  },deleteFile = FALSE)
  
  output$mt8 <- renderImage({
    m_i<- str_replace_all(rec(input$movie),"[[:punct:]] | [^[:ascii:]]"," ")
    filename <- normalizePath(file.path(
      paste("./www/img/",m_i[8], ".JPEG", sep = "")))
    
    list(src=filename)
  },deleteFile = FALSE)
  
  output$mt9 <- renderImage({
    m_i<- str_replace_all(rec(input$movie),"[[:punct:]] | [^[:ascii:]]"," ")
    filename <- normalizePath(file.path(
      paste("./www/img/",m_i[9], ".JPEG", sep = "")))
    
    list(src=filename)
  },deleteFile = FALSE)
  
  output$mt10 <- renderImage({
    m_i<- str_replace_all(rec(input$movie),"[[:punct:]] | [^[:ascii:]]"," ")
    filename <- normalizePath(file.path(
      paste("./www/img/",m_i[10], ".JPEG", sep = "")))
    
    list(src=filename)
  },deleteFile = FALSE)
  
  output$t1 <- renderText({
    str_replace_all(rec(input$movie),"[[:punct:]] | [^[:ascii:]]"," ")[1]
    
  })
  
  output$t2 <- renderText({
    str_replace_all(rec(input$movie),"[[:punct:]] | [^[:ascii:]]"," ")[2]
    
  })
  
  output$t3 <- renderText({
    str_replace_all(rec(input$movie),"[[:punct:]] | [^[:ascii:]]"," ")[3]
    
  })
  
  output$t4 <- renderText({
    str_replace_all(rec(input$movie),"[[:punct:]] | [^[:ascii:]]"," ")[4]
    
  })
  
  output$t5 <- renderText({
    str_replace_all(rec(input$movie),"[[:punct:]] | [^[:ascii:]]"," ")[5]
    
  })
  
  output$t6 <- renderText({
    str_replace_all(rec(input$movie),"[[:punct:]] | [^[:ascii:]]"," ")[6]
    
  })
  
  output$t7 <- renderText({
    str_replace_all(rec(input$movie),"[[:punct:]] | [^[:ascii:]]"," ")[7]
    
  })
  
  output$t8 <- renderText({
    str_replace_all(rec(input$movie),"[[:punct:]] | [^[:ascii:]]"," ")[8]
    
  })
  
  output$t9 <- renderText({
    str_replace_all(rec(input$movie),"[[:punct:]] | [^[:ascii:]]"," ")[9]
    
  })
  
  output$t10 <- renderText({
    str_replace_all(rec(input$movie),"[[:punct:]] | [^[:ascii:]]"," ")[10]
    
  })
})
    
  
  

