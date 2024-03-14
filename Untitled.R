library(rvest)
library(tidyverse)


hitter_stats <- read_csv("data_folder/mlbhitterstats2023.csv")
pitcher_stats <- read_csv("data_folder/mlbpitcherstats2023.csv")



hitter_stats <- hitter_stats %>% 
  mutate(fan_pts = 0.5*R + 1.5*`1B` + 3*`2B` + 4.5*`3B` + 6*HR + RBI + 1.5*SB - 1.5*CS +
           1.5*BB + 1.5*HBP - 1.5*SO)



pitcher_stats <- pitcher_stats %>% 
  rowwise() %>% 
  mutate(outs = ip_to_outs(IP),
         fan_pts = 4*W - 4*L + 8*CG + 8*ShO + outs + 4*SV - 1.5*H - 3*ER - 1.5*BB - 
           1.5*HBP + 2*SO + 4*HLD)




