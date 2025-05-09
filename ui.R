library(shiny)
library(ggplot2)
library(shinyjs)
library(tuneR)
library(DT)
library(readr) 
library(dplyr)

shinyUI(fluidPage(
  useShinyjs(),
  titlePanel("Statistical Significance?"),
  br(),
  em("Guess well and gain points; guess poorly and gain only shame (and also lose points)."),
  br(),
  br(),
  textInput("player_name", "Player Name:", "Player 1"),
  
  fluidRow(
    column(4,
           textOutput("PointsMaybe"),
           textOutput("WrongAnswers"),
           textOutput("PlotNumber"),
           hr(),
           
           uiOutput("gameOverMessage"),
           
           uiOutput("questionContainer"),
           br(),
           
           div(
             textOutput("Answer"),
             htmlOutput("r2Answer"),
             htmlOutput("pvalAnswer"),
             textOutput("Sass")
           ),
           hr()
    ),
    column(8,
           tags$audio(id = "coinSound", src = "smb_coin2.mp3", type = "audio/mp3", preload="auto"),
           tags$audio(id = "fireballSound", src = "smb_fireball2.mp3", type = "audio/mp3", preload="auto"),
           tags$audio(id = "owSound", src = "ow.mp3", type = "audio/mp3", preload="auto"),
           tags$audio(id = "gameOverSound", src = "smb_gameover.mp3", type = "audio/mp3", preload="auto"),
           tags$audio(id = "randomSound1", src = "smb_powerup.mp3", type = "audio/mp3", preload="auto"),
           tags$audio(id = "randomSound2", src = "smb_1-up.mp3", type = "audio/mp3", preload="auto"),
           tags$audio(id = "randomSound3", src = "smb_pipe.mp3", type = "audio/mp3", preload="auto"),
           tags$audio(id = "randomSound4", src = "smb_jump-super.mp3", type = "audio/mp3", preload="auto"),
           tags$audio(id = "randomSound5", src = "smb_stomp.mp3", type = "audio/mp3", preload="auto"),
           
           tabsetPanel(id = "mainTabs",
                       tabPanel("Game", 
                                plotOutput("scatterPlot"),
                                textOutput("r2value"),
                                textOutput("corr_value")
                       ),
                       tabPanel("High Scores",
                                h3("High Score Leaderboard"),
                                DTOutput("highScoresTable")
                       )
           )
    )
  )
))