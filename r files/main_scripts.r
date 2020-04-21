
clean_game_file <- function(raw_game){
  #df <- raw_game
  ### REMOVE USELESS COLUMNS
  raw_game$Timestamp.1 <- NULL
  raw_game$Speed <- NULL
  raw_game$Acceleration <- NULL
  ### REMOVE ROWS WITHOUT POSITION DATA
  raw_game <- raw_game[is.na(raw_game$X)==FALSE, ]
}

transform <- function(raw_game, start_time=100, end_time=0){

  
  ### GENERATE GAME GRAPHS ON THIS INTERVAL
  game <- getGameGraphs(raw_game, start_time, end_time)
  
  ### REMOVE EDGES CLOSE TO PLAYERS
  cl <- makeCluster(3, type="SOCK")
  game <- clusterApply(cl, game, removeEdges)
  stopCluster(cl)
  #for (i in seq(1, length(game))){
  #  game[[i]] <- removeEdges(game[[i]], 3)
    
  #}
  
  
  
  game
}

cnm_to_dataframe <- function(cnm_matrix, time_bin, team){
  cnm_matrix <- cnm_matrix[(1+(team-1)+((team-1)*10)):(11+(team-1)+((team-1)*10)), ]
  height <- dim(cnm_matrix)[1]
  width <- dim(cnm_matrix)[2]
  df <- data.frame()
  
  for(i in seq(1, width-time_bin)){
    df <- rbind(df, c(cnm_matrix[,i:(i+time_bin)]))
    
  }
  colnames(df) <- c(1:length(df))
  df
}

get_possession_labels <- function(ball_poss, time_bin, team, start_ts, end_ts){
  labels = c()
  
  for(i in seq((start_ts+(time_bin*100)), end_ts, 100)){
    bp <- head(ball_poss[ball_poss$Timestamp > i, ], 1)
    if (length(bp$Team) > 0){
      if (bp$Team == team){
        labels <- c(labels, 0)
      }
      else{
        labels <- c(labels, 1)
      }
    }
    else{
      bp <- tail(ball_poss[ball_poss$Timestamp <= i, ], 1)
      if (bp$Team == team){
        labels <- c(labels, 1)
      }
      else{
        labels <- c(labels, 0)
      }
    }
  }
  labels
}

get_attacking_labels <- function(attacking_timestamps, time_bin, team, start_ts, end_ts){
  labels = c()
  #attacking_timestamps <- attacking_timestamps[attacking_timestamps$Team == team, ]
  for(i in seq((start_ts+(time_bin*100)), end_ts, 100)){
    at <- head(attacking_timestamps[attacking_timestamps$Timestamp > i, ], 1)
    if (length(at$Event) > 0){
      if(at$Event == 'END'){
        if(at$Team == team){
          labels <- c(labels, 1)
        }
        else{
          labels <- c(labels, -1)
        }
      }
      else{
        labels <- c(labels, 0)
      }
    }
    else{
      labels <- c(labels, 0)
    }
  }
  labels
}


get_cnm <- function(game, path_to_save) {
  
  ### GRAPH MEASURES BY TEAM
  degreeT1 <- list()
  betweennessT1 <- list()
  closenessT1 <- list()
  degreeT2 <- list()
  betweennessT2 <- list()
  closenessT2 <- list()
  
  ### CALCULATE GRAPH MEASURES
  for (i in 1:length(game)){
    team1game <- delete.vertices(game[[i]], V(game[[i]])[V(game[[i]])$color == 'red' | V(game[[i]])$color == 'white'])
    degreeT1[[i]] <- centralization.degree(team1game)$res
    length(degreeT1[[i]]) <- 11
    betweennessT1[[i]] <- centralization.betweenness(team1game)$res
    length(betweennessT1[[i]]) <- 11
    closenessT1[[i]] <- centralization.closeness(team1game)$res
    length(closenessT1[[i]]) <- 11
    
    team2game <- delete.vertices(game[[i]], V(game[[i]])[V(game[[i]])$color == 'green' | V(game[[i]])$color == 'white'])
    degreeT2[[i]] <- centralization.degree(team2game)$res
    length(degreeT2[[i]]) <- 11
    betweennessT2[[i]] <- centralization.betweenness(team2game)$res
    length(betweennessT2[[i]]) <- 11
    closenessT2[[i]] <- centralization.closeness(team2game)$res
    length(closenessT2[[i]]) <- 11
  }
  
  ### WRITE GRAPH FILE
  folder = path_to_save
  try(dir.create(folder))
  saveRDS(game, file=paste(folder,'/GameGraphs.ggps', sep=''))
  
  ### WRITE MEASURE FILES
  write.table(data.frame(degreeT1), file = paste(folder, "/Team1_DegreeCentrality.csv", sep=''),sep=",",  col.names=FALSE)
  write.table(data.frame(betweennessT1), file = paste(folder, "/Team1_BetweennessCentrality.csv", sep=''),sep=",",  col.names=FALSE)
  write.table(data.frame(closenessT1), file = paste(folder, "/Team1_ClosenessCentrality.csv", sep=''),sep=",",  col.names=FALSE)
  
  write.table(data.frame(degreeT2), file = paste(folder, "/Team2_DegreeCentrality.csv", sep=''),sep=",",  col.names=FALSE)
  write.table(data.frame(betweennessT2), file = paste(folder, "/Team2_BetweennessCentrality.csv", sep=''),sep=",",  col.names=FALSE)
  write.table(data.frame(closenessT2), file = paste(folder, "/Team2_ClosenessCentrality.csv", sep=''),sep=",",  col.names=FALSE)
}

