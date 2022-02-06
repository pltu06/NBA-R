#Analyzing the Three Pointers Made in Recent Years
source("R-Prep.R")
source("DataAnalysisNBA.R")
#Do Three Pointers Made and Three Pointers Attempted Match Up?
games_3ptsa_sum <- 
  rs_game_logs %>% 
  select(idGame, fg3aTeam, yearSeason) %>%
  group_by(idGame, yearSeason) %>%
  summarise(fg3a_sum = sum(fg3aTeam)) %>%
  ungroup()

mean_3ptsa_season <- games_3ptsa_sum %>%
  group_by(yearSeason)%>%
  summarise(mean_3ptsa_sum = mean(fg3a_sum))%>%
  ungroup()

ggplot(data = mean_3ptsa_season, aes(x = yearSeason, y = mean_3ptsa_sum))+
  geom_point(data = games_3ptsa_sum, 
             aes(x = yearSeason, y = fg3a_sum), 
             position = position_jitter(.22))+
  geom_bar(stat = "identity", fill = "white", alpha = 1/3, color = "green")+
  geom_smooth(method = "lm", color = "red")+
  labs(x = "Season", y = "3 Pointers Attempted During Game")

ThreePointer_sum <-
  rs_game_logs %>%
  select(fg3aTeam, fg3mTeam, yearSeason) %>%
  group_by(yearSeason) %>%
  summarise(mean_3ptsa_sum, mean_3pts_sum) %>%
  ungroup()

# calculate three point attempts per team per game
fg3_data <- 
  rs_game_logs %>%
  select(
    season = yearSeason, 
    team = nameTeam, 
    teamid = idTeam, 
    idGame, fg3mTeam
    )

# A solution for having each teamid match up with one team name:
team_names <- 
  fg3_data %>%
  select(team, teamid) %>%
  distinct() %>%
  arrange(teamid) 

# These are the teams with repeat names
repeats <- 
  team_names %>%
  count(teamid) %>%
  filter(n>1)

# our chosen names (what they are named today)
new_names <- 
  repeats %>% 
  left_join(., team_names, by = "teamid") %>%
  mutate(
    name = case_when(
      teamid ==  1610612740 ~ "New Orleans Pelicans",
      teamid == 1610612746 ~ "Los Angeles Clippers" ,
      teamid == 1610612751 ~ "Brooklyn Nets" ,
      teamid == 1610612760 ~ "Oklahoma City Thunder" ,
      teamid == 1610612763 ~ "Memphis Grizzlies" ,
      teamid == 1610612766 ~ "Charlotte Hornets"
    )
  )

# Gathering data for the new names
replace_teams <- 
  new_names %>% 
  select(-team, -n) %>% 
  distinct() %>%
  rename(team = name) %>%
  left_join(., fg3_data, by ="teamid") %>%
  rename(team = team.x) %>%
  select(-team.y)

# appended the new data
fg3_data_newnames <- 
  fg3_data %>% 
  filter(teamid %nin% replace_teams$teamid) %>% # gets rid of repeat teams
  bind_rows(., replace_teams)

# THE plot
pj <- position_jitter(width = .2)
ggplot(fg3_data_newnames, aes(season, fg3mTeam)) +
  geom_point(position = pj, alpha = 1/3, shape = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Season", "Three-Pointers Made") +
  theme_classic() +
  facet_wrap(~team)

# Multilevel modeling of the NBA data
# look at blog post
fg3_mods <-  
  fg3_data_newnames %>%
  nest_by(team) %>%
  mutate(mod = list(lm(fg3mTeam ~ 1 + scale(season, scale = FALSE), data = data)))

summary(fg3_mods$mod[[2]]) 

#* 1. Go back and get to this point by averaging the number of 3-pointers made 
#* per team per season (hint use group_by(), and summarise() )
#* 2. using the broom package, extract all estimates into a df (hint use broom::tidy())
#* this includes: intercept, slope 
  








# A regression tutorial

#data("mtcars")

#ggplot(mtcars, aes(mpg, hp)) +
  #geom_point() +
  #geom_hline(yintercept = 150) + 
  #geom_smooth(method = "lm", se = TRUE)

# y ~ b0 + b1x1
# y = b + mx
#hp_mod <- lm(hp ~ 1 + mpg, data = mtcars)
#summary(hp_mod)
# hp = 324.08 + -8.83(mpg)

# modeling with mean centering
#hp_mod <- lm(hp ~ 1 + scale(mpg, scale = FALSE), data = mtcars)
#summary(hp_mod)

# demonstrating mean centering
#mtcars_mc <-
 # mtcars %>%
  #select(mpg, hp) %>%
  #mutate(mpg_mc = mpg - mean(mpg))

# demonstrates mean of mc is zero
#summary(mtcars_mc)

# multilevel modeling
# https://mattkmiecik.com/post-Multilevel-Modeling-in-R-with-NHL-Power-Play-Data.html

fg3_data_mod <- 
  fg3_data_newnames %>%
  nest_by(team) %>%
  mutate(mod = list(lm(fg3mTeam ~ 1 + scale(season, scale = FALSE), data = data)))

summary(fg3_data_mod$mod[[1]])

level1omni <-
  fg3_data_mod %>%
  summarise(broom::tidy(mod, conf.int = TRUE, conf.level = 0.95)) %>%
  ungroup() %>%
  mutate(
    sig = p.value < 0.05,
    term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "scale(season, scale = FALSE)" ~ "season"
    )
    )

ggplot(level1omni, aes(estimate, reorder(team, estimate), color = sig)) +
  geom_point(size = 2) +
  facet_wrap(~term, scales = "free")

ggplot(
  level1omni %>% filter(term == "Intercept"), 
  aes(estimate, reorder(team, estimate), color = sig)
  ) +
  geom_point(size = 2) +
  coord_cartesian(xlim = c(5, 10)) +
  geom_errorbarh(
    aes(xmin=conf.low, xmax=conf.high), 
    height=.2
  )

ggplot(
  level1omni %>% filter(term == "season"), 
  aes(estimate, reorder(team, estimate), color = sig)
) +
  geom_point(size = 2) +
  geom_errorbarh(
    aes(xmin=conf.low, xmax=conf.high), 
    height=.2
    )

# second level
level2omni <- 
  level1omni %>%
  nest_by(term) %>%
  mutate(
    mod = list(lm(estimate ~ 1, data = data))
    )

level2omni %>%
  summarise(broom::tidy(mod, conf.int = TRUE, conf.level = 0.95)) # conf.int = TRUE, conf.level = 0.95)

# What to work on this week (30 JAN 2022) ----
# also, push to github
# What do you think is driving this increase in 3 pointers?
# Look at data available in package to see what can be used to explain this effect
# Check out R markdown: https://rmarkdown.rstudio.com/lesson-1.html
  

# y = b0 + b1x1
# three pointers = -888 + .44(season)

# Assignments:
# Find a solution for having each teamid match up with one team name
# compare each team's slope using a plot
# tip: package broom
# function is called tidy()
# see: https://mattkmiecik.com/post-Multilevel-Modeling-in-R-with-NHL-Power-Play-Data.html

# lvl1 modeling
#   lvl1_data_spec %>%
#   nest_by(elec, ss) %>%
#   mutate(modA = list(lm(dB ~ 1 + stim_mc, data = data)))
#   
#   lvl1_est_spec <- 
#     lvl1_mod_spec %>%
#     summarise(broom::tidy(modA)) %>%
#     ungroup()

#
#
#
#
#
#ANALYZING OFFENSIVE REBOUNDS OVER 20 YEARS

games_oreb_sum <- rs_game_logs %>% 
  select(idGame, orebTeam, yearSeason) %>%
  group_by(idGame, yearSeason) %>%
  summarise(oreb_sum = sum(orebTeam)) %>%
  ungroup()

mean_oreb_season <- games_oreb_sum %>%
  group_by(yearSeason)%>%
  summarise(mean_oreb_sum = mean(oreb_sum))%>%
  ungroup()

ggplot(data = mean_oreb_season, aes(x = yearSeason, y = mean_oreb_sum))+
  geom_point(data = games_oreb_sum, 
             aes(x = yearSeason, y = oreb_sum), 
             position = position_jitter(.22))+
  geom_bar(stat = "identity", fill = "white", alpha = 1/3, color = "green")+
  geom_smooth(method = "lm", color = "red")+
  labs(x = "Season", y = "Offensive Rebounds per Game")

oreb_data <- 
  rs_game_logs %>%
  select(
    season = yearSeason, 
    team = nameTeam, 
    teamid = idTeam, 
    idGame, orebTeam
  )

# A solution for having each teamid match up with one team name:
team_names <- 
  oreb_data %>%
  select(team, teamid) %>%
  distinct() %>%
  arrange(teamid) 

# These are the teams with repeat names
repeats <- 
  team_names %>%
  count(teamid) %>%
  filter(n>1)

# our chosen names (what they are named today)
new_names <- 
  repeats %>% 
  left_join(., team_names, by = "teamid") %>%
  mutate(
    name = case_when(
      teamid ==  1610612740 ~ "New Orleans Pelicans",
      teamid == 1610612746 ~ "Los Angeles Clippers" ,
      teamid == 1610612751 ~ "Brooklyn Nets" ,
      teamid == 1610612760 ~ "Oklahoma City Thunder" ,
      teamid == 1610612763 ~ "Memphis Grizzlies" ,
      teamid == 1610612766 ~ "Charlotte Hornets"
    )
  )

# Gathering data for the new names
replace_teams <- 
  new_names %>% 
  select(-team, -n) %>% 
  distinct() %>%
  rename(team = name) %>%
  left_join(., oreb_data, by ="teamid") %>%
  rename(team = team.x) %>%
  select(-team.y)

# appended the new data
oreb_data_newnames <- 
  oreb_data %>% 
  filter(teamid %nin% replace_teams$teamid) %>% # gets rid of repeat teams
  bind_rows(., replace_teams)

pj <- position_jitter(width = .2)
ggplot(oreb_data_newnames, aes(season, orebTeam)) +
  geom_point(position = pj, alpha = 1/3, shape = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Season", "Offensive Rebounds") +
  theme_classic() +
  facet_wrap(~team)

oreb_mods <-  
  oreb_data_newnames %>%
  nest_by(team) %>%
  mutate(mod = list(lm(orebTeam ~ 1 + scale(season, scale = FALSE), data = data)))

level1orebomni <-
  oreb_mods %>%
  summarise(broom::tidy(mod, conf.int = TRUE, conf.level = 0.95)) %>%
  ungroup() %>%
  mutate(
    sig = p.value < 0.05,
    term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "scale(season, scale = FALSE)" ~ "season"
    )
  )

ggplot(level1orebomni, aes(estimate, reorder(team, estimate), color = sig)) +
  geom_point(size = 2) +
  facet_wrap(~term, scales = "free")

ggplot(
  level1orebomni %>% filter(term == "Intercept"), 
  aes(estimate, reorder(team, estimate), color = sig)
) +
  geom_point(size = 2) +
  coord_cartesian(xlim = c(9, 13)) +
  geom_errorbarh(
    aes(xmin=conf.low, xmax=conf.high), 
    height=.2
  )

ggplot(
  level1orebomni %>% filter(term == "season"), 
  aes(estimate, reorder(team, estimate), color = sig)
) +
  geom_point(size = 2) +
  geom_errorbarh(
    aes(xmin=conf.low, xmax=conf.high), 
    height=.2
  )

# second level
level2orebomni <- 
  level1orebomni %>%
  nest_by(term) %>%
  mutate(
    mod = list(lm(estimate ~ 1, data = data))
  )

level2orebomni %>%
  summarise(broom::tidy(mod, conf.int = TRUE, conf.level = 0.95))

#
#
#
#
#
#ANALYZING ASSISTS OVER 20 YEARS

games_ast_sum <- rs_game_logs %>% 
  select(idGame, astTeam, yearSeason) %>%
  group_by(idGame, yearSeason) %>%
  summarise(ast_sum = sum(astTeam)) %>%
  ungroup()

mean_ast_season <- games_ast_sum %>%
  group_by(yearSeason)%>%
  summarise(mean_ast_sum = mean(ast_sum))%>%
  ungroup()

ggplot(data = mean_ast_season, aes(x = yearSeason, y = mean_ast_sum))+
  geom_point(data = games_ast_sum, 
             aes(x = yearSeason, y = ast_sum), 
             position = position_jitter(.22))+
  geom_bar(stat = "identity", fill = "white", alpha = 1/3, color = "green")+
  geom_smooth(method = "lm", color = "red")+
  labs(x = "Season", y = "Assists per Game")

ast_data <- 
  rs_game_logs %>%
  select(
    season = yearSeason, 
    team = nameTeam, 
    teamid = idTeam, 
    idGame, astTeam
  )

# A solution for having each teamid match up with one team name:
team_names <- 
  ast_data %>%
  select(team, teamid) %>%
  distinct() %>%
  arrange(teamid) 

# These are the teams with repeat names
repeats <- 
  team_names %>%
  count(teamid) %>%
  filter(n>1)

# our chosen names (what they are named today)
new_names <- 
  repeats %>% 
  left_join(., team_names, by = "teamid") %>%
  mutate(
    name = case_when(
      teamid ==  1610612740 ~ "New Orleans Pelicans",
      teamid == 1610612746 ~ "Los Angeles Clippers" ,
      teamid == 1610612751 ~ "Brooklyn Nets" ,
      teamid == 1610612760 ~ "Oklahoma City Thunder" ,
      teamid == 1610612763 ~ "Memphis Grizzlies" ,
      teamid == 1610612766 ~ "Charlotte Hornets"
    )
  )

# Gathering data for the new names
replace_teams <- 
  new_names %>% 
  select(-team, -n) %>% 
  distinct() %>%
  rename(team = name) %>%
  left_join(., ast_data, by ="teamid") %>%
  rename(team = team.x) %>%
  select(-team.y)

# appended the new data
ast_data_newnames <- 
  ast_data %>% 
  filter(teamid %nin% replace_teams$teamid) %>% # gets rid of repeat teams
  bind_rows(., replace_teams)

pj <- position_jitter(width = .2)
ggplot(ast_data_newnames, aes(season, astTeam)) +
  geom_point(position = pj, alpha = 1/3, shape = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Season", "Assists") +
  theme_classic() +
  facet_wrap(~team)

ast_mods <-  
  ast_data_newnames %>%
  nest_by(team) %>%
  mutate(mod = list(lm(astTeam ~ 1 + scale(season, scale = FALSE), data = data)))

level1astomni <-
  ast_mods %>%
  summarise(broom::tidy(mod, conf.int = TRUE, conf.level = 0.95)) %>%
  ungroup() %>%
  mutate(
    sig = p.value < 0.05,
    term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "scale(season, scale = FALSE)" ~ "season"
    )
  )

ggplot(level1astomni, aes(estimate, reorder(team, estimate), color = sig)) +
  geom_point(size = 2) +
  facet_wrap(~term, scales = "free")

ggplot(
  level1astomni %>% filter(term == "Intercept"), 
  aes(estimate, reorder(team, estimate), color = sig)
) +
  geom_point(size = 2) +
  coord_cartesian(xlim = c(20, 25)) +
  geom_errorbarh(
    aes(xmin=conf.low, xmax=conf.high), 
    height=.2
  )

ggplot(
  level1astomni %>% filter(term == "season"), 
  aes(estimate, reorder(team, estimate), color = sig)
) +
  geom_point(size = 2) +
  geom_errorbarh(
    aes(xmin=conf.low, xmax=conf.high), 
    height=.2
  )

#
#
#
#
#
#ANALYZING THREE POINT PERCENT

games_Threepct_sum <- rs_game_logs %>% 
  select(idGame, pctFG3Team, yearSeason) %>%
  group_by(idGame, yearSeason) %>%
  summarise(Threepct_sum = sum(pctFG3Team)) %>%
  ungroup()

mean_Threepct_season <- games_Threepct_sum %>%
  group_by(yearSeason)%>%
  summarise(mean_Threepct_sum = mean(Threepct_sum))%>%
  ungroup()

ggplot(data = mean_Threepct_season, aes(x = yearSeason, y = mean_Threepct_sum))+
  geom_point(data = games_Threepct_sum, 
             aes(x = yearSeason, y = Threepct_sum), 
             position = position_jitter(.22))+
  geom_bar(stat = "identity", fill = "white", alpha = 1/3, color = "green")+
  geom_smooth(method = "lm", color = "red")+
  labs(x = "Season", y = "3-Point %")

#
#
#
#
#
#ANALYZING STEALS

games_stl_sum <- rs_game_logs %>% 
  select(idGame, stlTeam, yearSeason) %>%
  group_by(idGame, yearSeason) %>%
  summarise(stl_sum = sum(stlTeam)) %>%
  ungroup()

mean_stl_season <- games_stl_sum %>%
  group_by(yearSeason)%>%
  summarise(mean_stl_sum = mean(stl_sum))%>%
  ungroup()

ggplot(data = mean_stl_season, aes(x = yearSeason, y = mean_stl_sum))+
  geom_point(data = games_stl_sum, 
             aes(x = yearSeason, y = stl_sum), 
             position = position_jitter(.22))+
  geom_bar(stat = "identity", fill = "white", alpha = 1/3, color = "green")+
  geom_smooth(method = "lm", color = "red")+
  labs(x = "Season", y = "Steals")

