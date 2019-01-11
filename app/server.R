library(shiny)
library(DT)
#install necessary packages and load libraries
if (!require("tidyverse")) install.packages("tidyverse");library(tidyverse)
if (!require("jsonlite")) install.packages("jsonlite");library(jsonlite)
if (!require("fastDummies")) install.packages("fastDummies");library(fastDummies)
if (!require("class")) install.packages("class");library(class)
if (!require("recommenderlab")) install.packages("recommenderlab");library(recommenderlab)
if (!require("tm")) install.packages("tm");library(tm)
if (!require("text2vec")) install.packages("text2vec");library(text2vec)




#load data
movies <- read_csv("./data/tmdb_5000_movies.csv")
credits <- read_csv("./data/tmdb_5000_credits.csv")




# gather all genres under one movie 
movie_with_tag <- movies %>% filter(nchar(genres) > 2) %>% mutate(genres = lapply(genres, fromJSON)) %>% unnest(genres) %>% select(id, title, genres = name)

# do the same thing to cast members per movie 
credits_cast <- credits %>% select(-crew) %>% filter(nchar(cast) >2) %>% mutate(cast=lapply(cast,fromJSON)) %>% unnest(cast) %>% select(movie_id,title,cast=name) %>% mutate(id=movie_id)%>%mutate(movie_id=NULL) %>% mutate(casts = str_replace_all(cast, " ", "")) %>% select(id, title, cast = casts) %>% group_by(id, title) %>% mutate(cast = paste0(cast, collapse = " ")) %>% distinct()



movie_bag <- movies %>% filter(nchar(genres) > 2) %>% mutate(genres = lapply(genres, fromJSON)) %>% unnest(genres) %>% select(id,title,genres=name,overview) %>% group_by(id,title) %>% mutate(genres = paste0(genres, collapse =" ")) %>% distinct()

movie_bag2 <- inner_join(credits_cast,movie_bag,by="id")%>%select(id,title.x,cast,genres,overview)

# then repeat it for keywords per movie 
movie_with_keywords <- movies %>% filter(nchar(keywords) > 2) %>% mutate(keywords = lapply(keywords, fromJSON)) %>% unnest(keywords) %>% select(id, title, keywords = name)%>% group_by(id,title) %>% mutate(keywords=paste0(keywords, collapse=" ")) %>% distinct()

# repeat it for directors per movie 
movie_with_directors <- credits %>% select(-cast) %>% filter(nchar(crew) >2) %>% mutate(directors = lapply(crew, fromJSON)) %>% unnest(directors)%>% select(movie_id,title,job,directors=name) %>% filter(job == "Director")%>%mutate(id=movie_id)%>%mutate(movie_id=NULL)%>% mutate(director = str_replace_all(directors, " ", "")) %>% select(id, title, directors = director) %>% group_by(id, title) %>% mutate(directors = paste0(directors, collapse = " ")) %>% distinct()

# intergrate genres, cast and keywords together into a tibble and perform some preprocessing(transform words into lower case, remove punctuation)
movie_all <- inner_join(movie_with_keywords,movie_bag2, by="id") %>% select(-title.x) %>% mutate(overview=tolower(overview))%>%mutate(overview=removePunctuation(overview)) %>% mutate(keywords=tolower(keywords)) %>% mutate(cast=tolower(cast)) %>% mutate(genres=tolower(genres))

movie_a <- inner_join(movie_all,movie_with_directors, by=c("id","title"))%>% mutate(directors=tolower(directors))

# create a column with name "bag of words" and put genres, actors, keywords and overview into it to construct bag of words for each film
movie_words <- movie_a %>% group_by(title, id) %>% mutate(bag_of_words = paste(genres, keywords, cast, overview, directors, sep=" ",collapse=" "))

movie_full <- movie_with_tag %>% inner_join(movie_with_keywords, by = "id")

movie_full %>% group_by(keywords) %>% count() %>% arrange(desc(n))

movie_full %>% group_by(genres) %>% count() %>% arrange(desc(n))

#create document matrix and compute tf-idf cosine similarity 
it <- itoken(movie_words$bag_of_words,word_tokenizer)
v <- create_vocabulary(it)
vectorizer <- vocab_vectorizer(v)
dtm <- create_dtm(it,vectorizer)
model_tf <- TfIdf$new()
dtm_tfidf = model_tf$fit_transform(dtm)
cos_sim = sim2(x = dtm_tfidf, method = "cosine", norm = "l2")
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
    
  
  

