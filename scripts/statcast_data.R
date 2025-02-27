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

pitcher_result_stats <- data_results.filter %>% 
  group_by(pitcher_name, season) %>% 
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
                      2.1 * sum(events == "home_run")) / (pa)) %>%
  pivot_wider(names_from = season, values_from = c(pa, k_rate, bb_rate, barrel_rate, hard_hit_rate, wOBA)) %>% 
  filter(pa_2024 >= 15) %>% 
  mutate(k_diff = k_rate_2024 - k_rate_2023,
         bb_diff = bb_rate_2024 - bb_rate_2023,
         barrel_rate_diff = barrel_rate_2024 - barrel_rate_2023,
         hard_hit_diff = hard_hit_rate_2024 - hard_hit_rate_2023,
         woba_diff = wOBA_2024 - wOBA_2023) %>% 
  select(pitcher_name, k_diff, bb_diff, barrel_rate_diff, hard_hit_diff,
         woba_diff) %>% 
  arrange(desc(bb_diff))




### filter for pitching stat differences (k_rate, bb rate, zone %, whiff %, chase %)
pitcher_plate_stats <- data_pitches %>% 
  group_by(pitcher_name, season) %>% 
  summarise(whiff_rate = round(sum(whiff)/sum(swing), 3),
            o_zone_swing_rate = round(sum(chase, na.rm = T)/sum(actual_strike == 0, na.rm = T),3),
            swing_pct = round(sum(swing)/n(), 3), 
            zone_swing_rate = round(sum(zone_swing, na.rm = T)/sum(actual_strike, na.rm = T),3)) %>% 
  pivot_wider(names_from = season, values_from = c(whiff_rate, o_zone_swing_rate, swing_pct, zone_swing_rate)) %>% 
  mutate(whiff_diff = whiff_rate_2024 - whiff_rate_2023,
         chase_diff = o_zone_swing_rate_2024 - o_zone_swing_rate_2023,
         swing_diff = swing_pct_2024 - swing_pct_2023,
         zone_swing_diff = zone_swing_rate_2024 - zone_swing_rate_2023) %>% 
  filter(!is.na(whiff_diff)) %>% 
  select(pitcher_name, whiff_diff, chase_diff, swing_diff, zone_swing_diff)


### filter for pitch speed, movement by pitcher and pitch
data_pitches %>% 
  group_by(pitcher_name, season, pitch_name) %>% 
  summarise(avg_velo = mean(release_speed),
            avg_release_pos_x = mean(release_pos_x),
            avg_release_pos_y = mean(release_pos_y),
            avg_extension = mean(release_extension),
            avg_spin = mean(release_spin_rate))



