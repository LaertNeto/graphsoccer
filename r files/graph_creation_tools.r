

get_graph_from_df <- function(tt_df){
  library(igraph)
  fix_x = 54
  fix_y = 36
  font_size=6
  node_size=100
  ### GET TEAM NAMES
  teams <- unique(tt_df$Team)
  teams <- teams[teams != '']
  
  union2 <- function(g1, g2){
    
    #Internal function that cleans the names of a given attribute
    CleanNames <- function(g, target){
      #get target names
      gNames <- parse(text = (paste0(target,"_attr_names(g)"))) %>% eval 
      #find names that have a "_1" or "_2" at the end
      AttrNeedsCleaning <- grepl("(_\\d)$", gNames )
      #remove the _x ending
      StemName <- gsub("(_\\d)$", "", gNames)
      
      NewnNames <- unique(StemName[AttrNeedsCleaning])
      #replace attribute name for all attributes
      for( i in NewnNames){
        
        attr1 <- parse(text = (paste0(target,"_attr(g,'", paste0(i, "_1"),"')"))) %>% eval
        attr2 <- parse(text = (paste0(target,"_attr(g,'", paste0(i, "_2"),"')"))) %>% eval
        
        g <- parse(text = (paste0("set_",target,"_attr(g, i, value = ifelse(is.na(attr1), attr2, attr1))"))) %>%
          eval
        
        g <- parse(text = (paste0("delete_",target,"_attr(g,'", paste0(i, "_1"),"')"))) %>% eval
        g <- parse(text = (paste0("delete_",target,"_attr(g,'", paste0(i, "_2"),"')"))) %>% eval
        
      }
      
      return(g)
    }
    
    
    g <- igraph::union(g1, g2) 
    #loop through each attribute type in the graph and clean
    for(i in c("graph", "edge", "vertex")){
      g <- CleanNames(g, i)
    }
    
    return(g)
    
  }
  

  ball <- tt_df[tt_df$Player.Name == "ball", ]
  if (length(ball$Timestamp) > 0){
    ball_graph <- make_full_graph(1, directed = FALSE, loops = FALSE)
    V(ball_graph)$x <- ball$X + fix_x
    V(ball_graph)$y <- ball$Y + fix_y
    V(ball_graph)$color <- "white"
    V(ball_graph)$label.color<-"black"
    V(ball_graph)$label.font.size<-font_size
    V(ball_graph)$label.cex = 0.8
    V(ball_graph)$label.degree = pi/2
    V(ball_graph)$size = node_size
    V(ball_graph)$name = '0'
    #V(ball_graph)$player_name <- 'ball'
    #V(ball_graph)$shirt = 0
  }
  
  for (i in c(1,2))
  {
    #get only players on team T and current time_stamp
    aux <- tt_df[tt_df$Team == teams[i], ]
    n_of_players <- length(aux[,1])
    
    #buill graph
    g <- make_full_graph(n_of_players, directed = FALSE, loops = FALSE)
    V(g)$x <- aux$X + fix_x
    V(g)$y <- aux$Y + fix_y
    #V(g)$player_name<-aux$Player.Name
    #V(g)$shirt <- aux$Shirt
    
    #plotting details
    if (i == 1){
      V(g)$name <- strsplit(toString(seq(1,n_of_players)), ', ')[[1]]
      V(g)$color<-"green"
      E(g)$color<-"green"
    }
    else{
      V(g)$name <- strsplit(toString(seq(12,n_of_players+11)), ', ')[[1]]
      V(g)$color<-"red"
      E(g)$color<-"red"
    }
    
    V(g)$label.color<-"black"
    V(g)$label.font.size<-font_size
    V(g)$label.cex <- 0.8
    V(g)$label.degree <- pi/2
    V(g)$size <- node_size
    
    
    if (i == 1){
      game <- g
    }
    else{
      if (length(ball$Timestamp) > 0){
        game <- union2(game, g)
        game <- union2(game, ball_graph)
        #[[game_count]] <- game[[game_count]] + g + ball_graph
      }
      else{
        game <- union2(game, g)
        #game[[game_count]] <- game[[game_count]] + g
      }
    }
  }
  game
}

getGameGraphs <- function(df, from = 100, to = 0, node_size=100, font_size=6) {
  ### GET GAME DURATION
  if (to == 0){             
    to <- max(df$Timestamp)
  }
  data_frames <- list()
  i <- 1
  for (time_stamp in seq(from, to, 100)){
    data_frames[[i]] <- df[df$Timestamp == time_stamp, ]
    i <- i + 1
  }
  
  ### GET GRAPH/TIMESTAMP/TEAM
  cl <- makeCluster(3, type="SOCK")
  games <- clusterApply(cl, data_frames, get_graph_from_df)
  stopCluster(cl)
  games
}
