library(shiny)
library(ggplot2)
library(shinyjs)
library(tuneR)
library(DT)
library(readr) 
library(dplyr)

#UI
ui <- shinyUI(fluidPage(
  useShinyjs(),
  titlePanel("Statistical Significance?"),
  br(),
  em("Guess well and gain points; guess poorly and gain only shame (and also lose points)."),
  br(),
  br(),
  #Add player name input to the UI
  textInput("player_name", "Player Name:", "Player 1"),
  
  #Layout for all text and answers on left
  fluidRow(
    column(4,
           textOutput("PointsMaybe"),
           textOutput("WrongAnswers"),
           textOutput("PlotNumber"),
           hr(),
           
           #Game over message will appear here
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
    #Right side - Plot and tabs
    column(8,
           #Add audio tags for sounds
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
                                textOutput("corr_value") #correlation coefficient output
                       ),
                       tabPanel("High Scores",
                                h3("High Score Leaderboard"),
                                DTOutput("highScoresTable")
                       )
           )
    )
  )
))

#Server 
server <- function(input, output, session) {
  #File path for CSV storage
  scores_file <- "high_scores.csv"
  
  #Function to load high scores from CSV
  load_high_scores <- function() {
    if(file.exists(scores_file)) {
      #Read scores file
      tryCatch({
        high_scores <- read.csv(scores_file, stringsAsFactors = FALSE)
        
        high_scores <- high_scores %>%
          mutate(
            player_name = as.character(player_name),
            score = as.integer(score),
            completed_rounds = as.integer(completed_rounds),
            wrong_answers = as.integer(wrong_answers),
            date = as.character(date)
          )
        
        return(high_scores)
      }, error = function(e) {
        #Return empty dataframe if error
        message("Error loading high scores file: ", e$message)
        return(create_empty_scores_df())
      })
    } else {
      #Create empty dataframe if file doesn't exist
      return(create_empty_scores_df())
    }
  }
  
  #Helper function to create empty scores dataframe
  create_empty_scores_df <- function() {
    data.frame(
      player_name = character(),
      score = integer(),
      completed_rounds = integer(),
      wrong_answers = integer(),
      date = character(),
      stringsAsFactors = FALSE
    )
  }
  
  #Function to save high scores to CSV
  save_high_score <- function(player_name, score, completed_rounds, wrong_answers) {
    #Create new score entry
    new_score <- data.frame(
      player_name = player_name,
      score = score,
      completed_rounds = completed_rounds,
      wrong_answers = wrong_answers,
      date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
    )
    
    #Get existing scores
    current_scores <- load_high_scores()
    
    #Find if this player already has an entry
    player_idx <- which(current_scores$player_name == player_name)
    
    if(length(player_idx) > 0) {
      #Update existing entry if score is higher
      if(score > current_scores$score[player_idx[1]]) {
        current_scores$score[player_idx[1]] <- score
        current_scores$completed_rounds[player_idx[1]] <- completed_rounds
        current_scores$wrong_answers[player_idx[1]] <- wrong_answers
        current_scores$date[player_idx[1]] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      }
    } else {
      #Add new entry
      current_scores <- rbind(current_scores, new_score)
    }
    
    #Sort by score
    updated_scores <- current_scores %>%
      arrange(desc(score))
    
    #Save to CSV with error handling
    tryCatch({
      write.csv(updated_scores, scores_file, row.names = FALSE)
      message("Successfully saved to CSV")
    }, error = function(e) {
      message("Error saving to CSV: ", e$message)
      showNotification("Failed to save score locally.", type = "error")
    })
    
    return(updated_scores)
  }
  
  #Initialize high scores data
  high_scores_data <- reactiveVal(load_high_scores())
  
  #Refresh high score data every 30 seconds
  observe({
    invalidateLater(30000)
    high_scores_data(load_high_scores())
  })
  
  #Select random color
  newcolors <- function() {
    sample(c("pink", "salmon", "red", "green", "blue", "lightpink", "hotpink", 
             "greenyellow", "darkblue", "lightcoral", "darkgrey", 
             "purple", "darkgreen", "seagreen", "olivedrab", "orange2", "magenta"), 1)
  }
  
  #Generate more random scatterplot data with different patterns
  generate_scatter_data <- function() {
    #sample sizes
    sample_size <- sample(15:250, 1)
    
    #Choose a random pattern type
    pattern_type <- sample(1:5, 1)
    
    #Different data patterns
    if (pattern_type == 1) {
      #Linear relationship (positive)
      x <- runif(sample_size, min = 10, max = 100)
      noise_level <- runif(1, 5, 35)  # Variable noise
      y <- 2 * x + rnorm(sample_size, mean = 0, sd = noise_level)
    } else if (pattern_type == 2) {
      #Linear relationship (negative)
      x <- runif(sample_size, min = 10, max = 100)
      noise_level <- runif(1, 5, 35)
      y <- 100 - 1.5 * x + rnorm(sample_size, mean = 0, sd = noise_level)
    } else if (pattern_type == 3) {
      #Curved relationship
      x <- runif(sample_size, min = 10, max = 100)
      noise_level <- runif(1, 10, 40)
      y <- 0.03 * x^2 + rnorm(sample_size, mean = 0, sd = noise_level)
    } else if (pattern_type == 4) {
      #Clusters (no relationship)
      x <- c(runif(sample_size/3, min = 10, max = 30),
             runif(sample_size/3, min = 40, max = 60),
             runif(sample_size/3, min = 70, max = 100))
      y <- c(runif(sample_size/3, min = 30, max = 50),
             runif(sample_size/3, min = 30, max = 50),
             runif(sample_size/3, min = 30, max = 50))
    } else {
      #Random scatter (no relationship)
      x <- runif(sample_size, min = 10, max = 100)
      y <- runif(sample_size, min = 10, max = 100)
    }
    
    data <- data.frame(x = x, y = y)
    model <- lm(y ~ x, data = data)
    r_squared <- summary(model)$r.squared
    p_value_slope <- summary(model)$coefficients[2, 4]  #Extract p-value for slope
    
    #Calc correlation coefficient
    correlation <- cor(data$x, data$y)
    
    list(data = data, r_squared = r_squared, color = newcolors(), 
         p_value_slope = p_value_slope, correlation = correlation)
  }
  
  #Initialize the current data
  current_data <- reactiveVal(generate_scatter_data())
  
  #Render scatter plot
  output$scatterPlot <- renderPlot({
    data <- current_data()
    ggplot(data$data, aes(x = x, y = y)) +
      geom_point(color = data$color, size = 3) +
      geom_smooth(method = "lm", color = "black", se = FALSE) +
      labs(
        title = ("Is it Statistically Significant?"),
        x = paste("Sample Size:", length(data$data$y)), 
        y = "Observation Density"
      ) +
      theme_minimal()
  })
  

  
  #SASS
  bad <- c("Nope, WOW", "Oof try again", "Are you even playing?", "Naw that ain't it")
  good <- c("Wow you got this!", "Ooooh aren't you smart", "Well dang, that's not bad")
  
  sassifrass <- function(x) {
    ifelse(x == 1, sample(good, 1), sample(bad, 1))
  }
  
  #Determine correct answer based on slope p-value
  pstuff <- function(x) {
    ifelse(x$p_value_slope < 0.05, 1, 2)  #1 = significant, 2 = not significant
  }
  
  #Function to determine correct R² category
  r2_category <- function(r2) {
    if (r2 > 0.5) {
      return("high")
    } else if (r2 > 0.2) {
      return("medium")
    } else {
      return("low")
    }
  }
  
  #Function to determine correct p-value category
  pval_category <- function(pval) {
    if (pval < 0.01) {
      return("high_sig")
    } else if (pval < 0.05) {
      return("sig")
    } else {
      return("not_sig")
    }
  }
  
  #Function to play sound based on correct/incorrect answer
  playFeedbackSound <- function(is_correct) {
    if (is_correct) {
      runjs("document.getElementById('coinSound').play();")
    } else {
      runjs("document.getElementById('owSound').play();")
    }
  }
  
  #Function to play any sound by ID
  playSoundById <- function(sound_id) {
    runjs(paste0("
      try {
        document.getElementById('", sound_id, "').play();
      } catch(e) {
        console.log('Error playing sound: ' + e);
      }
    "))
  }
  
  # Function to update score after each question
  updateScorePerQuestion <- function() {
    player_name <- input$player_name
    if (nchar(player_name) == 0) {
      player_name <- "Anonymous"
    }
    
    #Update high score data
    updated_scores <- save_high_score(player_name, vals$sum, vals$completedRounds, vals$wrongAnswers)
    high_scores_data(updated_scores)
    
    #Show notification
    showNotification("Score updated!", type = "message", duration = 2)
  }
  
  #Initialize all reactive values
  guesses <- reactiveValues(sum = 0, points = 0)
  answers <- reactiveValues(
    ptalk = "", 
    sass = "", 
    r2feedback = "", 
    pvalfeedback = ""
  )
  vals <- reactiveValues(
    sum = 0,                #Points total
    pval = 200,             #Current p-value
    norm = "",              #Text description of significance
    currentQuestion = 1,    #Track which question is on
    answeredQ1 = FALSE,
    answeredQ2 = FALSE,
    answeredQ3 = FALSE,
    roundComplete = FALSE,
    wrongAnswers = 0,       #Track wrong answers
    completedRounds = 0,    #Track number of completed rounds
    gameOverSoundPlayed = FALSE, #Track if game over sound has been played
    gameOverNotified = FALSE #Track if user has been notified of game over
  )
  
  #Display completed rounds info
  output$PlotNumber <- renderText({
    paste("Completed Rounds:", vals$completedRounds)
  })
  
  #Display wrong answers count
  output$WrongAnswers <- renderText({
    paste("Wrong Answers:", vals$wrongAnswers)
  })
  
  #Ensure points start at 0
  output$PointsMaybe <- renderText({
    paste("Your Point Total:", vals$sum)
  })
  
  #Game Over message UI
  output$gameOverMessage <- renderUI({
    if(vals$wrongAnswers >= 4 && !vals$gameOverNotified) {
      
      
      #Play game over sound once
      if(!vals$gameOverSoundPlayed) {
        playSoundById("gameOverSound")
        vals$gameOverSoundPlayed <- TRUE
        vals$gameOverNotified <- TRUE
        
        tagList(
          h3("Game Over! You reached the maximum number of wrong answers."),
          actionButton("restartBtn", "Try Again")
        )
      }
      
      #Set game over notified flag
      vals$gameOverNotified <- TRUE
      
      #Display game over message
      div(
        style = "text-align: center; margin: 20px 0; padding: 15px; background-color: #ffcccc; border: 2px solid red; border-radius: 5px;",
        h3(style = "color: red; margin: 0;", "Warning: 4+ Wrong Answers"),
        p("You've reached 4 or more wrong answers, but you can continue playing!"),
        p(style = "font-style: italic;", "Each wrong answer affects your score.")
      )
    } else if(vals$wrongAnswers >= 4) {
      #Still display game over message if 4+ wrong answers
      div(
        style = "text-align: center; margin: 20px 0; padding: 15px; background-color: #ffcccc; border: 2px solid red; border-radius: 5px;",
        h3(style = "color: red; margin: 0;", "Warning: 4+ Wrong Answers"),
        p("You've reached 4 or more wrong answers, but you can continue playing!"),
        p(style = "font-style: italic;", "Each wrong answer affects your score.")
      )
    }
  })
  
  observeEvent(input$restartBtn, {
  vals$sum <- 0
  vals$pval <- 200
  vals$norm <- ""
  vals$currentQuestion <- 1
  vals$answeredQ1 <- FALSE
  vals$answeredQ2 <- FALSE
  vals$answeredQ3 <- FALSE
  vals$roundComplete <- FALSE
  vals$wrongAnswers <- 0
  vals$gameOverSoundPlayed <- FALSE
  vals$gameOverNotified <- FALSE

  #Reset data
  current_data(generate_scatter_data())
})

  #High scores table output
  output$highScoresTable <- renderDT({
    scores <- high_scores_data()
    
    if (nrow(scores) == 0) {
      datatable(
        data.frame(
          player_name = "No scores yet",
          score = NA,
          completed_rounds = NA,
          wrong_answers = NA,
          date = NA
        ),
        options = list(pageLength = 10),
        rownames = FALSE,
        colnames = c("Player Name", "Score", "Rounds Completed", "Wrong Answers", "Date")
      )
    } else {
      datatable(
        scores %>% 
          arrange(desc(score)) %>%  #Ensure sorting by score
          head(10),  #Show top 10 scores
        options = list(
          pageLength = 10,
          order = list(list(1, 'desc'))  #Sort by score column descending
        ),
        rownames = FALSE,
        colnames = c("Player Name", "Score", "Rounds Completed", "Wrong Answers", "Date")
      )
    }
  })
  
  #Check for 4+ wrong answers and play sound
  observe({
    if(vals$wrongAnswers >= 4 && !vals$gameOverSoundPlayed) {
      playSoundById("gameOverSound")
      vals$gameOverSoundPlayed <- TRUE
    }
  })
  
  #Question container
  output$questionContainer <- renderUI({
    if (vals$roundComplete) {
      #Show summary and start next round button
      tagList(
        h3("All questions answered!"),
        p("Your answers have been recorded."),
        actionButton("nextRound", "Start Next Round")
      )
    } else if (vals$currentQuestion == 1) {
      #Q1
      tagList(
        radioButtons("pvalguess", label = h3("1. Does this plot demonstrate statistical significance?"),
                     choices = list("Probably" = 1, "Unlikely" = 2),
                     selected = 1),
        actionButton("submitQ1", "Submit Answer")
      )
    } else if (vals$currentQuestion == 2) {
      #Q2
      tagList(
        h3("2. How would you rate the R² value?"),
        radioButtons("r2guess", label = NULL,
                     choices = list(
                       "Strong relationship (R² > 0.5)" = "high",
                       "Moderate relationship (0.2 < R² ≤ 0.5)" = "medium",
                       "Weak relationship (R² ≤ 0.2)" = "low"
                     ),
                     selected = "medium"),
        actionButton("submitQ2", "Submit Answer")
      )
    } else if (vals$currentQuestion == 3) {
      #Q3
      tagList(
        h3("3. What would you estimate the p-value to be?"),
        radioButtons("pvalEstimate", label = NULL,
                     choices = list(
                       "Highly significant (p < 0.01)" = "high_sig",
                       "Significant (0.01 ≤ p < 0.05)" = "sig",
                       "Not significant (p ≥ 0.05)" = "not_sig"
                     ),
                     selected = "sig"),
        actionButton("submitQ3", "Submit Answer")
      )
    }
  })
  
  #Q1 submit handler
  observeEvent(input$submitQ1, {
    current <- current_data()
    vals$pval <- current$p_value_slope
    correct_answer <- pstuff(current)
    sig_points <- ifelse(correct_answer == input$pvalguess, 1, -1)
    
    #Update wrong answers count if incorrect
    if(correct_answer != input$pvalguess) {
      vals$wrongAnswers <- vals$wrongAnswers + 1
    }
    
    #Store Q1 results
    vals$q1Points <- sig_points
    vals$answeredQ1 <- TRUE
    
    #Update point total after Q1
    vals$sum <- vals$sum + sig_points
    
    #Update score in high scores immediately after Q1
    updateScorePerQuestion()
    
    #Play feedback sound
    if(correct_answer == input$pvalguess) {
      playSoundById("coinSound")
    } else {
      playSoundById("owSound")
    }
    
    #Process feedback for Q1
    if (vals$pval < 0.051) {
      vals$norm <- "statistically significant"
    } else {
      vals$norm <- "NOT statistically significant"
    }
    answers$ptalk <- paste("The real p-value for the slope was", round(vals$pval, 3), "; this sample is", vals$norm)
    
    #Move to next question
    vals$currentQuestion <- 2
  })
  
  #Q2 submit handler
  observeEvent(input$submitQ2, {
    current <- current_data()
    vals$r2 <- current$r_squared
    correct_r2 <- r2_category(vals$r2)
    r2_points <- ifelse(correct_r2 == input$r2guess, 1, -1)
    
    #Update wrong answers count if incorrect
    if(correct_r2 != input$r2guess) {
      vals$wrongAnswers <- vals$wrongAnswers + 1
    }
    
    #Store Q2 results
    vals$q2Points <- r2_points
    vals$answeredQ2 <- TRUE
    
    #Update point total after Q2
    vals$sum <- vals$sum + r2_points
    
    #Update score in high scores immediately after Q2
    updateScorePerQuestion()
    
    #Play feedback sound
    if(correct_r2 == input$r2guess) {
      playSoundById("coinSound")
    } else {
      playSoundById("owSound")
    }
    
    #Process feedback for Q2
    r2_text <- paste("The R² value is", round(vals$r2, 2), "which indicates a")
    if (correct_r2 == "high") {
      r2_text <- paste(r2_text, "<span style='color:blue; font-weight:bold'>strong relationship</span>")
    } else if (correct_r2 == "medium") {
      r2_text <- paste(r2_text, "<span style='color:orange; font-weight:bold'>moderate relationship</span>")
    } else {
      r2_text <- paste(r2_text, "<span style='color:red; font-weight:bold'>weak relationship</span>")
    }
    r2_text <- paste(r2_text, "- Your answer was", ifelse(correct_r2 == input$r2guess, "correct (+1)", "incorrect (-1)"))
    answers$r2feedback <- r2_text
    
    #Move to next question
    vals$currentQuestion <- 3
  })
  
  #Q3 submit handler
  observeEvent(input$submitQ3, {
    current <- current_data()
    vals$pval <- current$p_value_slope
    correct_pval_cat <- pval_category(vals$pval)
    pval_est_points <- ifelse(correct_pval_cat == input$pvalEstimate, 1, -1)
    
    #Update wrong answers count if incorrect
    if(correct_pval_cat != input$pvalEstimate) {
      vals$wrongAnswers <- vals$wrongAnswers + 1
    }
    
    #Store Q3 results
    vals$q3Points <- pval_est_points
    vals$answeredQ3 <- TRUE
    
    #Update point total after Q3
    vals$sum <- vals$sum + pval_est_points
    
    #Update score in high scores immediately after Q3
    updateScorePerQuestion()
    
    #Play feedback sound
    if(correct_pval_cat == input$pvalEstimate) {
      playSoundById("coinSound")
    } else {
      playSoundById("owSound")
    }
    
    #Process feedback for Q3
    pval_text <- paste("The actual p-value", round(vals$pval, 3), "is")
    if (correct_pval_cat == "high_sig") {
      pval_text <- paste(pval_text, "<span style='color:green; font-weight:bold'>highly significant (p < 0.01)</span>")
    } else if (correct_pval_cat == "sig") {
      pval_text <- paste(pval_text, "<span style='color:blue; font-weight:bold'>significant (0.01 ≤ p < 0.05)</span>")
    } else {
      pval_text <- paste(pval_text, "<span style='color:red; font-weight:bold'>not significant (p ≥ 0.05)</span>")
    }
    pval_text <- paste(pval_text, "- Your answer was", ifelse(correct_pval_cat == input$pvalEstimate, "correct (+1)", "incorrect (-1)"))
    answers$pvalfeedback <- pval_text
    
    #Set sass message
    round_points <- vals$q1Points + vals$q2Points + vals$q3Points
    vals$sassmaster <- ifelse(round_points > 0, sample(good, 1), sample(bad, 1))
    answers$sass <- vals$sassmaster
    
    vals$roundComplete <- TRUE
    
    #Update round count at the end of the round
    vals$completedRounds <- vals$completedRounds + 1
    
    #Play a random sound at the end of every round
    random_sound_id <- paste0("randomSound", sample(1:5, 1))
    playSoundById(random_sound_id)
  })
  
  #Handle "Start Next Round"
  observeEvent(input$nextRound, {
    current_data(generate_scatter_data())
    
    #Reset for new round
    vals$currentQuestion <- 1
    vals$answeredQ1 <- FALSE
    vals$answeredQ2 <- FALSE
    vals$answeredQ3 <- FALSE
    vals$roundComplete <- FALSE
    
    #Clear feedback
    answers$ptalk <- ""
    answers$sass <- ""
    answers$r2feedback <- ""
    answers$pvalfeedback <- ""
    
    #Play a sound for new round
    playSoundById("fireballSound")
  })
  
  #Game restart handler (for fresh start if user wants)
  observeEvent(input$restartGame, {
    #Reset game state
    vals$wrongAnswers <- 0
    vals$sum <- 0
    vals$completedRounds <- 0
    vals$gameOverSoundPlayed <- FALSE
    vals$gameOverNotified <- FALSE
    
    #Generate new plot
    current_data(generate_scatter_data())
    
    #Reset for new game
    vals$currentQuestion <- 1
    vals$answeredQ1 <- FALSE
    vals$answeredQ2 <- FALSE
    vals$answeredQ3 <- FALSE
    vals$roundComplete <- FALSE
    
    #Clear feedback
    answers$ptalk <- ""
    answers$sass <- ""
    answers$r2feedback <- ""
    answers$pvalfeedback <- ""
    
    #Update score
    updateScorePerQuestion()
    
    #Play a sound for new game
    playSoundById("fireballSound")
    
    #Switch back to game tab
    updateTabsetPanel(session, "mainTabs", selected = "Game")
  })
  
  #Add view high scores button handler
  observeEvent(input$viewHighScores, {
    updateTabsetPanel(session, "mainTabs", selected = "High Scores")
  })
  
  #Output results
  output$Answer <- renderText({answers$ptalk})
  output$r2Answer <- renderUI({HTML(answers$r2feedback)})
  output$pvalAnswer <- renderUI({HTML(answers$pvalfeedback)})
  output$Sass <- renderText({answers$sass})
}

#Run app
shinyApp(ui = ui, server = server)