library(tidyverse)
library(zoo)
library(ggplot2)

## load id table
chadwick_player_lu_table <- chadwick_player_lu()

key_ids <- chadwick_player_lu_table %>% 
  mutate(name = paste(name_first, name_last)) %>% 
  select(name, key_fangraphs)

## plot moving averages of stats
### PITCHERS
data <- pitcher_game_logs_fg(key_ids %>% 
                       filter(name == "Braxton Garrett") %>% drop_na(key_fangraphs) %>% 
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

