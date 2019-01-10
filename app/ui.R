#First step, import the file and clean the data 


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


library(shinyWidgets)
library(shiny)
library(DT)

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
    # Show a plot of the generated distribution
  fluidRow(
    column(width = 12, div(style = "height:40px;"))),
  
    fluidRow(
      column(width = 4, offset = 5, tableOutput("mt"))
       
    )
)

