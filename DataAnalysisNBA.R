#R NBA Data Analysis
#Patrick Tu
#Started on 11/28/21
#Purpose: Analyze data from the NBA

#Prepare R Workspace----
source("R-Prep.R")
#source("R-Prepro.R") #If you want to refresh data frame run this

#Load data----
load("Output/rs_game_logs.RData")

#Measuring points scored in games in the NBA in the past two decades
games_pts_sum <- rs_game_logs %>% 
  select(idGame, ptsTeam, yearSeason) %>%
  group_by(idGame) %>%
  summarise(pts_sum = sum(ptsTeam)) %>%
  ungroup()

games_pts_sum <-rs_game_logs %>% 
  select(idGame, ptsTeam, yearSeason) %>%
  group_by(idGame, yearSeason) %>%
  summarise(pts_sum = sum(ptsTeam)) %>%
  ungroup()

ggplot(data = games_pts_sum, aes(x = yearSeason, y = pts_sum))+
  geom_point(position = position_jitter(width = .1), alpha = 1/3)

mean_pts_season <-games_pts_sum %>%
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
games_3pts_sum <- rs_game_logs %>% 
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
  labs(x = "Season", y = "3 Pointers Made During Game")

#Looking at number of free throws shot in the NBA for the past two decades
games_fta_sum <- rs_game_logs %>% 
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

# dive deeper into three point shooting analysis by visualizing each team 
# sep over 2 decades or so
# make this a new script
# next time I will go over linear modeling in R.
# tip: try to use facet_wrap() in ggplot2

