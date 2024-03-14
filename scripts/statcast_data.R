## statcast data
library(baseballr)
library(lubridate)
library(tidyverse)

## read in the data

data_pitches <- read_rds("data_folder/data_pitches.rds")

## filter for balls in play
data_pitches %>% 
  filter(events != "") -> data_results


data_pitches.filter <- data_pitches 

data_results.filter <- data_pitches.filter %>% 
  filter(events != "")

data_results.filter %>% 
  group_by(batter_name, season) %>% 
  summarise(pa = n(),
            k_rate = sum(events == "strikeout")/n(),
            bb_rate = sum(events == "walk")/n(),
            barrel_rate = sum(barrel, na.rm = T)/sum(!is.na(barrel)),
            hard_hit_rate = sum(hard_hit, na.rm = T)/sum(!is.na(hard_hit)),
            wOBA = (0.690 * sum(events == "walk") +
                      0.72 * sum(events == "hit_by_pitch") +
                      .89* sum(events == "single") +
                      1.27 * sum(events == "double") +
                      1.62 * sum(events == "triple") +
                      2.1 * sum(events == "home_run")) / (pa),
            pull_percent = sum(hit_type == "pull", na.rm = T)/sum(type == "X"),
            center_percent = sum(hit_type == "center", na.rm = T)/sum(type == "X"),
            oppo_percent = sum(hit_type == "oppo", na.rm = T)/sum(type == "X")) %>%
  pivot_wider(names_from = season, values_from = c(pa, k_rate, bb_rate, barrel_rate, hard_hit_rate, wOBA, pull_percent,
                                                   center_percent, oppo_percent)) %>% 
  filter(pa_2024 >= 15) %>% 
  mutate(k_diff = k_rate_2024 - k_rate_2023,
         bb_diff = bb_rate_2024 - bb_rate_2023,
         barrel_rate_diff = barrel_rate_2024 - barrel_rate_2023,
         hard_hit_diff = hard_hit_rate_2024 - hard_hit_rate_2023,
         woba_diff = wOBA_2024 - wOBA_2023,
         pull_diff = pull_percent_2024-pull_percent_2023,
         center_diff = center_percent_2024 - center_percent_2023,
         oppo_diff = oppo_percent_2024 - oppo_percent_2023) %>% 
  select(batter_name, k_diff, bb_diff, barrel_rate_diff, hard_hit_diff,
         woba_diff, pull_diff, center_diff, oppo_diff) %>% 
  arrange(desc(bb_diff)) %>% View




### filter for pitching stat differences (k_rate, bb rate, zone %, whiff %, chase %)
data_pitches %>% 
  grou





