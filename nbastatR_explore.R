#Exploring the nbastatR function
install.packages("devtools")
devtools::install_github("abresler/nbastatR")
library(nbastatR)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

assign_nba_players()
assign_nba_teams()
bulls_annual_stats <- teams_annual_stats(team_ids = 1610612741)

games <- game_logs(seasons = 2021, 
          league = "NBA", 
          result_types = "team", 
          season_types = "Regular Season")
library("dplyr")

#Measuring points scored in games in the NBA in the past two decades
games_pts_sum <-games %>% 
  select(idGame, ptsTeam, yearSeason) %>%
  group_by(idGame) %>%
  summarise(pts_sum = sum(ptsTeam)) %>%
  ungroup()

library(ggplot2)

ggplot(data = games_pts_sum, aes(x = 2021, y = pts_sum))+
  geom_point(position = position_jitter())

ggplot(data = games_pts_sum, aes(x = pts_sum))+
  geom_histogram(binwidth = 5)

years <- seq(2000, 2021, 1)
  
games <- game_logs(seasons = years, 
                   league = "NBA", 
                   result_types = "team", 
                   season_types = "Regular Season")

games_pts_sum <- games %>% 
  select(idGame, ptsTeam, yearSeason) %>%
  group_by(idGame, yearSeason) %>%
  summarise(pts_sum = sum(ptsTeam)) %>%
  ungroup()

ggplot(data = games_pts_sum, aes(x = yearSeason, y = pts_sum))+
  geom_point(position = position_jitter(width = .1), alpha = 1/3)

mean_pts_season <- games_pts_sum %>%
  group_by(yearSeason)%>%
  summarise(mean_pts_sum = mean(pts_sum))%>%
  ungroup()

ggplot(data = mean_pts_season, aes(x = yearSeason, y = mean_pts_sum))+
  geom_point(data = games_pts_sum, 
             aes(x = yearSeason, y = pts_sum), 
             position = position_jitter(.22))+
  geom_bar(stat = "identity", fill = "white", alpha = 1/3, color = "red")+
  geom_smooth(method = "lm")


#Measuring 3 pointers throughout the years in the NBA
years <- seq(2000, 2021, 1)

games <- game_logs(seasons = years, 
                   league = "NBA", 
                   result_types = "team", 
                   season_types = "Regular Season")

games_3pts_sum <-games %>% 
  select(idGame, fg3mTeam, yearSeason) %>%
  group_by(idGame, yearSeason) %>%
  summarise(fg3m_sum = sum(fg3mTeam)) %>%
  ungroup()

mean_3pts_season <- games_3pts_sum %>%
  group_by(yearSeason)%>%
  summarise(mean_3pts_sum = mean(fg3m_sum))%>%
  ungroup()

ggplot(data = mean_3pts_season, aes(x = yearSeason, y = mean_3pts_sum))+
  geom_point(data = games_3pts_sum, 
             aes(x = yearSeason, y = fg3m_sum), 
             position = position_jitter(.22))+
  geom_bar(stat = "identity", fill = "white", alpha = 1/3, color = "green")+
  geom_smooth(method = "lm", color = "red")+
  labs(x = "Season", y = "3 Pointers Shot During Game")

#Looking at number of free throws shot in the NBA for the past two decades
years <- seq(2000, 2021, 1)

games <- game_logs(seasons = years, 
                   league = "NBA", 
                   result_types = "team", 
                   season_types = "Regular Season")

games_fta_sum <-games %>% 
  select(idGame, ftaTeam, yearSeason) %>%
  group_by(idGame, yearSeason) %>%
  summarise(fta_sum = sum(ftaTeam)) %>%
  ungroup()

mean_fta_season <- games_fta_sum %>%
  group_by(yearSeason)%>%
  summarise(mean_fta_sum = mean(fta_sum))%>%
  ungroup()

ggplot(data = mean_fta_season, aes(x = yearSeason, y = mean_fta_sum))+
  geom_point(data = games_fta_sum, 
             aes(x = yearSeason, y = fta_sum), 
             position = position_jitter(.22))+
  geom_bar(stat = "identity", fill = "white", alpha = 1/3, color = "green")+
  geom_smooth(method = "lm", color = "red")+
  labs(x = "Season", y = "Free Throws Shot During Game")
