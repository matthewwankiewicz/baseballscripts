library(tidyverse)
library(zoo)
library(ggplot2)

key_ids %>% 
  filter(name == "Joe Kelly")

key_ids <- chadwick_player_lu_table %>% 
  select(name, key_fangraphs)


pitcher_game_logs_fg(23429, year = 2023) %>% 
  colnames()


### PITCHERS

data <- pitcher_game_logs_fg(key_ids %>% 
                       filter(name == "Cole Ragans") %>% drop_na(key_fangraphs) %>% 
                       pull(key_fangraphs), year = 2023) %>% 
  mutate(OUTS = ip_to_outs(IP),
         QS = ifelse(IP >= 6 & ER <= 3, 1, 0),
         PTS = 4*W - 4*L + 8*CG + 8*ShO + 0.5*OUTS - 1.5*H - 3*ER -
           1.5*BB - 1.5*HBP + 2.5*SO + 6*QS + 6*SV + 4*HLD,
         month = format(as.Date(Date, format="%Y-%m-%d"),"%m"),
         five_game = (zoo::rollmean(PTS, k = 5, align = "left", fill = NA)),
         ten_game = (zoo::rollmean(PTS, k = 1, align = "left", fill = NA)),
         three_game = (zoo::rollmean(PTS, k = 3, align = "left", fill = NA)),
         Date = as.Date(Date)); data %>% 
  ggplot() +
  geom_hline(yintercept = mean(data$PTS)) +
  geom_line(aes(x = Date, y = five_game, color = "FiveGmAvg")) +
  geom_line(aes(x = Date, y = ten_game, color = "OneGmAvg")) +
  geom_line(aes(x = Date, y = three_game, color = "ThreeGmAvg")) +
  ggtitle(label = "Rolling Avgs") +
  labs(color = "Legend") +
  theme_minimal()


format(as.Date(pitching_stats$Date, format="%Y-%m-%d"),"%m")


### BATTERS
data <- batter_game_logs_fg(key_ids %>%
                              filter(name == "Jeremy Peña") %>%
                              pull(key_fangraphs), year = 2023) %>%
  mutate(CYC = ifelse(`1B` >= 1 & `2B` >= 1 & `3B` >= 1 & HR >= 1, 1, 0),
         PTS = 0.5*R + 1.5*`1B` + 3*`2B` + 4.5*`3B` + 6*HR + RBI + 2*SB - 2*CS + 1.5*BB + 1.5*HBP + 1.5*IBB - 1.5*SO + 5*CYC,
         five_game = (zoo::rollmean(PTS, k = 5, align = "left", fill = NA)),
         ten_game = (zoo::rollmean(PTS, k = 10, align = "left", fill = NA)),
         three_game = (zoo::rollmean(PTS, k = 3, align = "left", fill = NA)),
         fifteen_game = (zoo::rollmean(PTS, k = 15, align = "left", fill = NA)),
         Date = as.Date(Date)); data %>%
  select(Date, Opp, PTS, five_game, ten_game, fifteen_game,three_game) %>%
  ggplot() +
  geom_line(aes(x = Date, y = five_game, color = "FiveGmAvg")) +
  geom_line(aes(x = Date, y = ten_game, color = "TenGmAvg")) +
  geom_line(aes(x = Date, y = fifteen_game, color = "FifteenGmAvg"), linewidth = .85) +
  geom_line(aes(x = Date, y = three_game, color = "ThreeGmAvg")) +
  geom_hline(yintercept = mean(data$PTS)) +
  ggtitle(label = "Rolling Avgs") +
  labs(color = "Legend",
       subtitle = paste("Season Avg:", round(mean(data$PTS), 3), sep = " ")) +
  theme_minimal() +
  ylim(c(-5,10)); paste("Min 5gm:", min(data$five_game, na.rm = T), "Max 5gm:", max(data$five_game,na.rm=T))

sum(data$PTS)


# Install and load required packages
install.packages("zoo")
install.packages("ggplot2")
library(zoo)
library(ggplot2)

# Sample dataset
scores <- c(10, 15, 12, 18, 20, 22, 19, 16, 14, 17, 21, 23, 24, 26)

# Five-game rolling average
five_game_avg <- rollmean(scores, k = 5, align = "right", fill = NA)

# Ten-game rolling average
ten_game_avg <- rollmean(scores, k = 10, align = "right", fill = NA)

# Create a data frame
data <- data.frame(Game = 1:length(scores), Scores = scores,
                   FiveGameAvg = five_game_avg, TenGameAvg = ten_game_avg)

ggplot(data) +
  geom_line(aes(x = Game, y = Scores, color = "Scores"), linetype = "solid") +
  geom_line(aes(x = Game, y = FiveGameAvg, color = "Five-Game Avg"), linetype = "dashed") +
  geom_line(aes(x = Game, y = TenGameAvg, color = "Ten-Game Avg"), linetype = "dotted") +
  labs(x = "Game", y = "Scores", color = "Legend") +
  ggtitle("Rolling Averages") +
  theme_minimal()


data <- batter_game_logs_fg(23003, year = 2023) %>%
  mutate(CYC = ifelse(`1B` >= 1 & `2B` >= 1 & `3B` >= 1 & HR >= 1, 1, 0),
         PTS = 0.5*R + 1.5*`1B` + 3*`2B` + 4.5*`3B` + 6*HR + RBI + 2*SB - 2*CS + 1.5*BB + 1.5*HBP + 1.5*IBB - 1.5*SO + 5*CYC,
         five_game = (zoo::rollmean(PTS, k = 5, align = "left", fill = NA)),
         ten_game = (zoo::rollmean(PTS, k = 10, align = "left", fill = NA)),
         three_game = (zoo::rollmean(PTS, k = 3, align = "left", fill = NA)),
         fifteen_game = (zoo::rollmean(PTS, k = 15, align = "left", fill = NA)),
         Date = as.Date(Date))



### BETTING

library(Lahman)
library(fangraphsR)
library(dplyr)
library(ggplot2)
library(zoo)


player_table %>% 
  mutate(name = paste(name_first, name_last, sep = " ")) %>% 
  select(name, key_mlbam, key_fangraphs) %>% 
  filter(name == "Luis García")


get_plots <- function(named, stat, years = 2023) {
  key_ids <- player_table %>% 
    drop_na(key_mlbam) %>% 
    mutate(name = paste(name_first, name_last, sep = " ")) %>% 
    select(name, key_fangraphs)
  player <- key_ids %>%
    filter(name == named) %>%
    pull(key_fangraphs) %>% .[1]
  
  data <- batter_game_logs_fg(player, year = years) %>%
    mutate(CYC = ifelse(`1B` >= 1 & `2B` >= 1 & `3B` >= 1 & HR >= 1, 1, 0),
           PTS = 0.5 * R + 1.5 * `1B` + 3 * `2B` + 4.5 * `3B` + 6 * HR + RBI + 2 * SB - 2 * CS + 1.5 * BB + 1.5 * HBP + 1.5 * IBB - 1.5 * SO + 5 * CYC,
           tb = `1B` + 2 * `2B` + 3 * `3B` + 4 * HR,
           Date = as.Date(Date)) %>% 
    filter(AB > 0)
  
  data <- data %>%
    mutate(
      five_game = zoo::rollmean(!!sym(stat), k = 5, align = "left", fill = NA),
      ten_game = zoo::rollmean(!!sym(stat), k = 10, align = "left", fill = NA),
      fifteen_game = zoo::rollmean(!!sym(stat), k = 15, align = "left", fill = NA),
      three_game = zoo::rollmean(!!sym(stat), k = 3, align = "left", fill = NA)
    )
  
  
  season_avg <- mean(data[[stat]], na.rm = TRUE)
  

  ggplot(data) +
    geom_line(aes(x = Date, y = five_game, color = "FiveGmAvg")) +
    geom_line(aes(x = Date, y = ten_game, color = "TenGmAvg")) +
    geom_line(aes(x = Date, y = fifteen_game, color = "FifteenGmAvg"), linewidth = 0.85) +
    geom_line(aes(x = Date, y = three_game, color = "ThreeGmAvg")) +
    geom_line(aes(x = Date, y = !!sym(stat), color = "PlayerStat"), linetype = "dashed") +
    geom_hline(yintercept = season_avg) +
    ggtitle("Rolling Averages") +
    labs(color = "Legend") +
    theme_minimal() +
    labs(subtitle = paste("Min 5gm:", min(data$five_game, na.rm = T), "| Max 5gm:", max(data$five_game,na.rm=T),
                          "| Season Avg:", round(season_avg, 3), "| Current 15gm avg:", round(data$fifteen_game[1], 3),
                          "| Current 10gm avg:", round(data$ten_game[1],3)))
}



get_plots("Tony Kemp", "PTS", 2023)






