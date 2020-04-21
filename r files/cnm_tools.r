getCentrality <- function(game, sorted){
  degreeT1 <- list()
  degreeT2 <- list()
  result <- list()
  for (i in 1:length(game)){
    team1game <- delete.vertices(game[[i]], V(game[[i]])[V(game[[i]])$color == 'red' | V(game[[i]])$color == 'white'])
    team2game <- delete.vertices(game[[i]], V(game[[i]])[V(game[[i]])$color == 'green' | V(game[[i]])$color == 'white'])
    degreeT1[[i]] <- centralization.degree(team1game, normalized = TRUE)$res
    degreeT2[[i]] <- centralization.degree(team2game, normalized = TRUE)$res
    if(sorted){
      degreeT1[[i]] <- sort(degreeT1[[i]], method = 'quick')
      degreeT2[[i]] <- sort(degreeT2[[i]], method = 'quick')
    }
    length(degreeT1[[i]]) <- 11
    length(degreeT2[[i]]) <- 11
    result[[i]] <- (c(degreeT1[[i]],degreeT2[[i]]))
  }
  result
}

getBetweenness <- function(game, sorted){
  degreeT1 <- list()
  degreeT2 <- list()
  result <- list()
  for (i in 1:length(game)){
    team1game <- delete.vertices(game[[i]], V(game[[i]])[V(game[[i]])$color == 'red' | V(game[[i]])$color == 'white'])
    team2game <- delete.vertices(game[[i]], V(game[[i]])[V(game[[i]])$color == 'green' | V(game[[i]])$color == 'white'])
    degreeT1[[i]] <- centralization.betweenness(team1game, normalized = TRUE, directed = FALSE)$res
    degreeT2[[i]] <- centralization.betweenness(team2game, normalized = TRUE, directed = FALSE)$res
    if(sorted){
      degreeT1[[i]] <- sort(degreeT1[[i]], method = 'quick')
      degreeT2[[i]] <- sort(degreeT2[[i]], method = 'quick')
    }
    length(degreeT1[[i]]) <- 11
    length(degreeT2[[i]]) <- 11
    result[[i]] <- (c(degreeT1[[i]],degreeT2[[i]]))
  }
  result
}

getCloseness <- function(game, sorted){
  degreeT1 <- list()
  degreeT2 <- list()
  result <- list()
  for (i in 1:length(game)){
    team1game <- delete.vertices(game[[i]], V(game[[i]])[V(game[[i]])$color == 'red' | V(game[[i]])$color == 'white'])
    team2game <- delete.vertices(game[[i]], V(game[[i]])[V(game[[i]])$color == 'green' | V(game[[i]])$color == 'white'])
    degreeT1[[i]] <- centralization.closeness(team1game, normalized = TRUE)$res
    degreeT2[[i]] <- centralization.closeness(team2game, normalized = TRUE)$res
    if(sorted){
      degreeT1[[i]] <- sort((degreeT1[[i]]), method = 'quick')
      degreeT2[[i]] <- sort((degreeT2[[i]]), method = 'quick')
    }
    length(degreeT1[[i]]) <- 11
    length(degreeT2[[i]]) <- 11
    result[[i]] <- (c(degreeT1[[i]],degreeT2[[i]]))
  }
  result
}

getEfficiency <- function(game, sorted){
  degreeT1 <- list()
  degreeT2 <- list()
  result <- list()
  for (i in 1:length(game)){
    team1game <- delete.vertices(game[[i]], V(game[[i]])[V(game[[i]])$color == 'red' | V(game[[i]])$color == 'white'])
    team2game <- delete.vertices(game[[i]], V(game[[i]])[V(game[[i]])$color == 'green' | V(game[[i]])$color == 'white'])
    degreeT1[[i]] <- efficiency(team1game,type='nodal', weights = NULL, use.parallel = FALSE)
    degreeT2[[i]] <- efficiency(team2game,type='nodal', weights = NULL, use.parallel = FALSE)
    
    if(sorted){
      degreeT1[[i]] <- sort((degreeT1[[i]]), method = 'quick')
      degreeT2[[i]] <- sort((degreeT2[[i]]), method = 'quick')
    }
    length(degreeT1[[i]]) <- 11
    length(degreeT2[[i]]) <- 11
    result[[i]] <- (c(degreeT1[[i]],degreeT2[[i]]))
  }
  result
  
}