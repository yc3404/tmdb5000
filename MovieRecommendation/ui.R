#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  fluidRow(
  column(width = 4, div(style = "height:100px;")), 
  column(width = 4, div(style = "height:100px;"), h1("Movie Recommendation", align =
                                                       "center")), 
         column(width = 4, div(style = "height:100px;"))),
  
  fluidRow(
    column(width = 12, div(style = "height:10px;"))),
  
  fluidRow(
    column(width = 5, div(style = "height:100px;")),
    column(width = 4, div(style = "height:100px;"), 
          selectInput("movie", "Movie Recommendation", choices = movie_words$title),
          submitButton("find"))),
    
    # Show a plot of the generated distribution
  fluidRow(
    column(width = 12, div(style = "height:40px;"))),
  
    fluidRow(
      column(width = 4, offset = 5, tableOutput("mt"))
       
    )
))

