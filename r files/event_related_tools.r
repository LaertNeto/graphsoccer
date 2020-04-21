

getPlayerList <- function(game_df){
  #path_to_df <- 'C:/Users/LaertNeto/Desktop/TCC/1_EL_XIV.csv'
  pl <- (unique(select(game_df, Team, Player.Name, Shirt)))
  pl <- pl[pl$Player.Name != 'ball', ]
  pl
}

get_ball_possession <- function(raw_events, player_list, start_ts){
  raw_events$Player <- factor(raw_events$Player, levels=levels(player_list$Player.Name))
  raw_events <- raw_events[grepl('Transition', raw_events$Event, fixed=TRUE), ]
  current_possession <- player_list$Team[player_list$Player.Name == raw_events[1,'Player']]
 
  possession_dataframe <- data.frame("Timestamp" = start_ts, "Team" = current_possession)
  
  
  for (i in 2:length(raw_events$X)){
    team <- player_list$Team[player_list$Player.Name == raw_events[i,'Player']]
    time_stamp <- raw_events[i, 'End_Ts..ms.']
    if (team!= current_possession){
      current_possession <- team
      possession_dataframe <- rbind(possession_dataframe, list(time_stamp, team))
    }
  }
  possession_dataframe
}

get_attacking_timestamps <- function(df, events, start_time, end_time){
  field_size = 108
  left_side_threshold <- -(field_size/2) + field_size/3
  right_side_threshold <- (field_size/2) - field_size/3
  teams <- unique(df$Team)
  teams <- teams[teams != '']
  
  first_df <- df[df$Timestamp == start_time, ]
  team1_minx <- min(first_df[first_df$Team == teams[[1]], ]$X)
  team2_minx <- min(first_df[first_df$Team == teams[[2]], ]$X)
  
  ball_poss <- get_ball_possession(events, getPlayerList(df), start_time)
  
  if (team1_minx < team2_minx){
    left_team <- 1
    right_team <- 2
  }
  else{
    left_team <- 2
    right_team <- 1
  }
  
  ball_df <- df[df$Team == '', ]
  ball_df <- ball_df[ball_df$Timestamp >= start_time, ]
  ball_df <- ball_df[ball_df$Timestamp <= end_time, ]
  attacks <- data.frame("Team"='team', "Timestamp"=0, "Event"='event', stringsAsFactors = FALSE)
  ongoing <- FALSE
  current_attacking_team <- NULL
  for (i in seq(start_time, end_time, 100)){
    ball_pos <- ball_df[ball_df$Timestamp == i, ]$X
    if(length(ball_pos) != 0){
      if(ball_pos < left_side_threshold){
        bp <- tail(ball_poss[ball_poss$Timestamp < i, ], 1)
        
        if((bp$Team == teams[[right_team]])){
          if (!ongoing){
            attacks <- rbind(attacks, list(teams[[right_team]], i, 'START'))
            current_attacking_team <- right_team
            ongoing <- TRUE
          }
        }else{
          if(!is.null(current_attacking_team)){
            attacks <- rbind(attacks, list(teams[[current_attacking_team]], i, 'END'))
            current_attacking_team <- NULL
          }
          ongoing <- FALSE
        }
      }
      else{
        if(ball_pos > right_side_threshold)
        {
          bp <- tail(ball_poss[ball_poss$Timestamp < i, ], 1)
          
          if((bp$Team == teams[[left_team]])){
            if (!ongoing){
              attacks <- rbind(attacks, list(teams[[left_team]], i, 'START'))
              current_attacking_team <- left_team
              ongoing <- TRUE
            }
          }
          else{
            if(!is.null(current_attacking_team)){
              attacks <- rbind(attacks, list(teams[[current_attacking_team]], i, 'END'))
              current_attacking_team <- NULL
            }
            ongoing <- FALSE
          }
        }
        else
        {
          if(!is.null(current_attacking_team)){
            attacks <- rbind(attacks, list(teams[[current_attacking_team]], i, 'END'))
            current_attacking_team <- NULL
          }
          ongoing <- FALSE
        }
      }
    }
    
  }
  attacks[attacks$Team != 'team', ]
  
}
