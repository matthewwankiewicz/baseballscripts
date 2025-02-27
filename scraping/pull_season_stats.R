library(rvest)
library(tidyverse)


hitter_stats <- read_csv("data_folder/mlbhitterstats2023.csv")
pitcher_stats <- read_csv("data_folder/mlbpitcherstats2023.csv")



## scrape hitter leaderboards
bat24 <- baseballr::fg_bat_leaders(startseason = 2024, endseason = 2024)
bat23 <- baseballr::fg_bat_leaders(startseason = 2023, endseason = 2023)

## combine
batting_stats <- rbind(bat23, bat24)

## calculate fantasy points
batting_stats <- batting_stats %>% 
  mutate(PTS = 0.5*R + 1.5*`1B` + 3*`2B` + 4.5*`3B` + 6*HR + 1*RBI + 
           1.5*SB - 1.5*CS + 1*BB - 1*SO)


batting_stats <- batting_stats %>% 
  select(Season, G, AB, PA, `1B`, `2B`, `3B`,
           HR, R, RBI, BB, SO, PTS)


## scrape pitcher leaderboards
pitch24 <- fg_pitcher_leaders(startseason = 2024, endseason = 2024)
pitch23 <- fg_pitcher_leaders(startseason = 2023, endseason = 2023)


## combine
pitching_stats <- rbind(pitch23, pitch24)

## calculate fantasy points
pitching_stats <- pitching_stats %>% 
  rowwise() %>% 
  mutate(IPouts = ip_to_outs(IP),
         PTS = 4*W - 4*L + 8*CG + 8*ShO + 6*SV +
           1*IPouts - 1.5*H - 3*ER - 1.5*BB + 2*SO + 4*HLD + 6*QS) %>% 
  select(Season, G, GS, ShO, SV, IPouts,
         H, ER, BB, SO, HLD, QS, PTS)


full_stats <- bind_rows(pitching_stats, batting_stats)

