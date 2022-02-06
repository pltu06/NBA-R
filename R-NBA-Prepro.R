#R NBA Preprocessing
#Patrick Tu
#Started on 11/28/21
#Purpose: Generate data frames for analysis

#Preparing the workspace----
source("R-Prep.R")

##This is all the regular season game logs from 2000 to 2021
years <- seq(2000, 2021, 1)
rs_game_logs <- 
  game_logs(
    seasons = years, 
    league = "NBA", 
    result_types = "team", 
    season_types = "Regular Season"
    ) 
save(rs_game_logs, file = "Output/rs_game_logs.RData")
