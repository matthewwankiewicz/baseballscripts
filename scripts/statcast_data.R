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

result_stats <- data_results.filter %>% 
  group_by(batter_name) %>% 
  summarise(pa = n(),
            k_rate = sum(events == "strikeout")/n(),
            bb_rate = sum(events == "walk")/n(),
            barrel_rate = sum(barrel, na.rm = T)/sum(!is.na(barrel)),
            hard_hit_rate = sum(hard_hit, na.rm = T)/sum(!is.na(hard_hit)))

