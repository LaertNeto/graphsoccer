current_loaded_cnm <- read.csv('C:/Users/LaertNeto/Desktop/Laert - TCC (Shiny)/data/efficiency_sorted', header = FALSE)
load(file='C:/Users/LaertNeto/Desktop/Laert - TCC (Shiny)/data/game1_first_half')
load(file='C:/Users/LaertNeto/Downloads/game.ggps')
current_loaded_game <<- items[[1]]
current_loaded_event <<- items[[2]]
ts_min <<- items[[3]]
ts_max <<- items[[4]]
current_loaded_df <<- items[[5]]
current_loaded_cnm[is.na(current_loaded_cnm)] = 0
edge_detection(current_loaded_cnm)

raw_game_df <- read.csv('C:/Users/LaertNeto/Desktop/TCC/1_EL_XIV.csv')
game_df <- clean_game_file(raw_game_df)
events <- read.csv('C:/Users/LaertNeto/Desktop/TCC/1_EL_XIV_Event.csv')
game <- transform(game_df, start_time = 100, end_time=2000)


image(t(apply(as.matrix(current_loaded_cnm[1:11,]),2,rev)), col  = rgb(red = (0:32)/32, green=0, blue=0), main = "Team A", yaxt = "n")

ksize = 3
kernel <- matrix(1, nrow = 11, ncol = 121)
i <- mmand::opening(as.matrix(current_loaded_cnm[1:11, ]),kernel)
i <- mmand::closing(as.matrix(current_loaded_cnm[1:11, ]),kernel)
i <- mmand::erode(as.matrix(current_loaded_cnm[1:11, ]),kernel)
i <- mmand::dilate(as.matrix(current_loaded_cnm[1:11, ]),kernel)
i <- mmand::meanFilter(as.matrix(current_loaded_cnm[1:11, ]),kernel)
image(t(apply(i,2,rev)), col  = rgb(red = (0:32)/32, green=0, blue=0), main = "Team A", yaxt = "n", xaxt = "n")
par(new=TRUE)
plot()

raw_game_df <- read.csv('C:/Users/LaertNeto/Desktop/TCC/1_EL_XIV.csv')
game_df <- clean_game_file(raw_game_df)
ball_poss <- get_ball_possession(events, getPlayerList(game_df))


### set up data
cnm <- cnm_to_dataframe(as.matrix(current_loaded_cnm), 20, 1)
attacking_ts <- get_attacking_timestamps(game_df, events, items[[3]], items[[4]])
al <- get_attacking_labels(attacking_ts, 20, 'NL001', items[[3]], items[[4]])

cnm$label <- al
cnm[is.na(cnm)] = 0


teams <- unique(current_loaded_df$Team)
teams <- teams[teams != '']
t <- 20
###CREATE ROWS WITH CNM MEASURES
cnm1 <- cnm_to_dataframe(as.matrix(current_loaded_cnm), t, 1)
cnm2 <- cnm_to_dataframe(as.matrix(current_loaded_cnm), t, 2)
###ADD ATTACKING LABELS FOR EACH ROW
if (input$attackingCheckbox){
  print('Start attack')
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