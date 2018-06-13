library(shiny)
library(shinythemes)
library(stringi)
library(tm)
library(plyr)
library(stringr)
library(sentimentr)
library(plotrix)
library(tidytext)
library(ggplot2)
library(wordcloud2)
library(twitteR)
library(tuber)
library(dplyr)



## Set the working directory

setwd("C:/Users/Richie/Desktop/NLP")

shinyUI(fluidPage(theme=shinytheme('cerulean'),
        
        navbarPage(title="Text Mining",id="nav",
                   
                   tabPanel("Sentiment Analysis of Trending Entrepreneurs 2018",value="TextMining",
                            
                            
                            fluidRow(style="background-color:white;",column(6,
                                            helpText(h3("Select Trending Entrepreneurs of 2018")),
                                            selectInput(inputId = "Entrepreneurs",label="Select Entrepreneurs",
                                                        choices=c("SundarPichai","Jackma","ElonMusk",
                                                                  "BillGates","JeffBezos","NaveenJain"),
                                                        selected="ElonMusk"),
                                            br(),
                                            
                                           
                                            imageOutput(outputId="image_plot",width="400",height="350"),
                                            
                                            textOutput(outputId ="quote")),
                                     column(6,
                                            helpText(h2("Word Cloud")),
                                                   wordcloud2Output(outputId = "wordcloud2"))
                                            
                                            
                                            
                                          )),
                                     fluidRow(
                                       column(6,
                                            helpText(h3("Different Category of Emotion (3D Pie)")),
                                            radioButtons(inputId = "lexicon",label="choose lexicon dictionary",
                                                         choices=c("nrc","bing"),selected="nrc"),
                                            plotOutput(outputId = "pie3d")),
                                            
                                            
                                            column(6,helpText(h3("Word Count")),
                                            plotOutput(outputId = "word_count"))
                                            
                                            
                                          ),
                                     fluidRow(column(6,helpText(h3("Polarity Bar Plot")),
                                              plotOutput(outputId = "Bar_Plot"))
                                              ,
                                          column(6,
                                          helpText(h3("Trending Videos with Number of likes and views")),
                                          tableOutput(outputId = "table_views"))
                                            
                                            ),
                     
                   
                   tabPanel("Data Source and Code",value="Resources",
                            fluidRow(column(12)))
                )
            )
        
)
