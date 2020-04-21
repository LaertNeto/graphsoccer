list.of.packages <- c("shiny", "plotrix", "igraph", "dplyr", "stringr", "parallel", "snow", "brainGraph", "OpenImageR", "mmand")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(shiny)
library(plotrix)
library(igraph)
library(dplyr)
library(stringr)
library(parallel)
library(brainGraph)
library(OpenImageR)
library(mmand)
source("main_scripts.r")
source("event_related_tools.r")
source("distance_tools.r")
source("drawing_tools.r")
source("edge_removal_tools.r")
source("cnm_tools.r")
source("graph_creation_tools.r")

options(shiny.maxRequestSize=200*1024^2) 

game_is_loaded <- FALSE
current_loaded_game <- NULL
current_loaded_cnm <- NULL
current_loaded_df <- NULL
current_loaded_event <- NULL
current_loaded_image <- NULL
ts_min <- NULL
ts_max <- NULL

server <- function(input, output, session){

 
    
  observeEvent(input$loadGame, {
    
    inFile <- input$gameFile
    inEventFile <- input$eventFile
    if (is.null(inFile)){
      output$transform_info <- renderText({ 
        paste("Please upload the game file.")
      })
      return(NULL)
    }
    if (is.null(inEventFile)){
      output$transform_info <- renderText({ 
        paste("You uploaded the game file, now please upload the event file.")
      })
      return(NULL)
    }
    ##### READ AND CLEAN FILES
    raw_game_df <- read.csv(inFile$datapath)
    game_df <- clean_game_file(raw_game_df)
    current_loaded_df <<- game_df
    current_loaded_event <<- read.csv(inEventFile$datapath)
    ts_min <<- input$loadTime[1]
    ts_max <<- input$loadTime[2]
    current_loaded_event <<- current_loaded_event[current_loaded_event$Start_Ts..ms. >= ts_min, ]
    current_loaded_event <<- current_loaded_event[current_loaded_event$End_Ts..ms. <= ts_max, ]
    ##### GET MEASURES
    ### TRANSFORM POSITIONAL DATA -> GRAPH DATA
    
    current_loaded_game <<- transform(game_df, start_time = input$loadTime[1], end_time=input$loadTime[2])
    updateSliderInput(session, 'timestamp', max=length(current_loaded_game))
    output$transform_info <- renderText({ 
      paste("Game transformed and loaded successfully. You can download the transformed file.")
    })
    game_is_loaded <<- TRUE
    
    
  })
  
  output$currentFrame <- renderText({ 
    paste("Frame:", ts_min+(100*input$timestamp))
  })
  output$game <- downloadHandler(
    filename = function(){
      paste("game","ggps",sep=".")
    },
    content = function(file){
      items <- list(current_loaded_game, current_loaded_event, ts_min, ts_max, current_loaded_df)
      save(items, file=file)
    }
  )
  
  observeEvent(input$loadTransformedGame, {
    inFile <- input$transformedFile
    if (is.null(inFile))
      return(NULL)
    
    load(file=inFile$datapath)
   
    current_loaded_game <<- items[[1]]
    current_loaded_event <<- items[[2]]
    ts_min <<- items[[3]]
    ts_max <<- items[[4]]
    current_loaded_df <<- items[[5]]
    updateSliderInput(session, 'timestamp', max=length(current_loaded_game))
    game_is_loaded <<- TRUE
  })
  
  output$degreeCentrality <- downloadHandler(
    filename = function(){
      paste("degreeCentrality","csv",sep=".")
    },
    content = function(file){
      r <- getCentrality(current_loaded_game, input$sortedCheckbox)
      write.table(data.frame(r), file = file,sep=",",  col.names=FALSE, row.names=FALSE)
    }
  )
  
  output$betweennessCentrality <- downloadHandler(
    filename = function(){
      paste("betweennessCentrality","csv",sep=".")
    },
    content = function(file){
      r <- getBetweenness(current_loaded_game, input$sortedCheckbox)
      write.table(data.frame(r), file = file,sep=",",  col.names=FALSE, row.names=FALSE)
    }
  )
  
  output$closenessCentrality <- downloadHandler(
    filename = function(){
      paste("closenessCentrality","csv",sep=".")
    },
    content = function(file){
      r <- getCloseness(current_loaded_game, input$sortedCheckbox)
      write.table(data.frame(r), file = file,sep=",",  col.names=FALSE, row.names=FALSE)
    }
  )
  
  output$efficiency <- downloadHandler(
    filename = function(){
      paste("efficiency","csv",sep=".")
    },
    content = function(file){
      r <- getEfficiency(current_loaded_game, input$sortedCheckbox)
      write.table(data.frame(r), file = file,sep=",",  col.names=FALSE, row.names=FALSE)
    }
  )
  output$filteredMeasures <- downloadHandler(
    filename = function(){
      paste("filteredMeasures","csv",sep=".")
    },
    content = function(file){
      write.table(data.frame(current_loaded_image), file = file,sep=",",  col.names=FALSE, row.names=FALSE)
    }
  )
  

  
  output$gameTS <- renderPlot({
    try(drawGame(current_loaded_game[[input$timestamp]]), silent = TRUE)
  })
  

  
  observeEvent(input$teamA_click$x, {
    updateSliderInput(session, 'timestamp', value = round(input$teamA_click$x * length(current_loaded_cnm)))
  })
  observeEvent(input$teamB_click$x, {
    updateSliderInput(session, 'timestamp', value = round(input$teamB_click$x * length(current_loaded_cnm)))
  })
  
  observeEvent(input$nextButton, {
    updateSliderInput(session, 'timestamp', value = input$timestamp+1)
  })
  observeEvent(input$prevButton, {
    updateSliderInput(session, 'timestamp', value = input$timestamp-1)
  })
  
  
  output$featureButton <- downloadHandler(
    filename = function(){
      paste("filteredMeasures","csv",sep=".")
    },
    content = function(file){
      
      teams <- unique(current_loaded_df$Team)
      teams <- teams[teams != '']
      t <- input$sizeTime
      ###CREATE ROWS WITH CNM MEASURES
      cnm1 <- cnm_to_dataframe(as.matrix(current_loaded_cnm), t, 1)
      cnm2 <- cnm_to_dataframe(as.matrix(current_loaded_cnm), t, 2)
      ###ADD ATTACKING LABELS FOR EACH ROW
      if (input$attackingCheckbox){
        attacking_ts <- get_attacking_timestamps(current_loaded_df, current_loaded_event, ts_min, ts_max)
        al1 <- get_attacking_labels(attacking_ts, t, teams[[1]], ts_min, ts_max)
        al2 <- get_attacking_labels(attacking_ts, t, teams[[2]], ts_min, ts_max)
        cnm1$attack <- al1
        cnm2$attack <- al2
      }
      
      ###ADD BALL POSSESSION LABELS FOR EACH ROW
      if (input$ballPossCheckbox){
        ball_poss <- get_ball_possession(current_loaded_event, getPlayerList(current_loaded_df), ts_min)
        al1 <- get_possession_labels(ball_poss, t, teams[[1]], ts_min, ts_max)
        al2 <- get_possession_labels(ball_poss, t, teams[[2]], ts_min, ts_max)
        cnm1$ball_poss <- al1
        cnm2$ball_poss <- al2
        
      }
      cnm <- rbind(cnm1, cnm2)
      write.table(cnm, file = file,sep=",",  col.names=TRUE, row.names=FALSE)
    }
  )
  
  observeEvent(input$cnmLoad, {
    inFile <- input$cnmFile
    if (is.null(inFile))
      return(NULL)
    imageFilter <- input$selectedFilter
    current_loaded_cnm<<-read.csv(inFile$datapath, header = FALSE)
    current_loaded_cnm[is.na(current_loaded_cnm)] = 0
    
    teams <- unique(current_loaded_df$Team)
    teams <- teams[teams != '']
    
    #ball_poss <- get_ball_possession(current_loaded_event, getPlayerList(current_loaded_df), ts_min)
    #ball_poss <- get_attacking_timestamps(current_loaded_df, current_loaded_event, ts_min, ts_max)
    #ball_poss<- ball_poss[ball_poss$Event == 'START', ]
    #greenTeam <- ball_poss[ball_poss$Team == teams[[1]],]
    #redTeam <- ball_poss[ball_poss$Team == teams[[2]],]
    
    #green_timestamps_ball_poss <- (greenTeam$Timestamp - ts_min) / (ts_max - ts_min)
    #red_timestamps_ball_poss <- (redTeam$Timestamp - ts_min) / (ts_max - ts_min)
    
    output$plotGVR_TA <- renderPlot({
      try(getImage(current_loaded_cnm[1:11, ], imageFilter, 'green'))
      #axis(side=3, at = green_timestamps_ball_poss, labels = FALSE , lwd.ticks=4, col = "green", las =2)
      #axis(side=3, at = red_timestamps_ball_poss, labels = FALSE , lwd.ticks=4, col = "red", las =2)
    })
    output$plotGVR_TB <- renderPlot({
      try(getImage(current_loaded_cnm[12:22, ], imageFilter, 'red'))
      #axis(side=3, at = green_timestamps_ball_poss, labels = FALSE , lwd.ticks=4, col = "green", las =2)
      #axis(side=3, at = red_timestamps_ball_poss, labels = FALSE , lwd.ticks=4, col = "red", las =2)
    })
    
    i1 <- applyFilter(current_loaded_cnm[1:11, ], imageFilter)
    i2 <- applyFilter(current_loaded_cnm[12:22, ], imageFilter)
    current_loaded_image <<- rbind(i1, i2)
  })

 
  output$transform_info <- renderText({ 
    paste("Choose a file and the part of the game you wish to transform, and press the button")
  })
  
}
