## statcast data
library(baseballr)
library(lubridate)
library(tidyverse)

# Get today's date and yesterday's date
current_date <- Sys.Date() 
yesterday <- current_date - days(1)

# Define the start date as March 30th or the next day after the last saved data
start_date <- ymd("2023-03-30")

# Create an empty list to store the date ranges
date_ranges <- list()

# Loop to generate date ranges from the start date to the current date
while (start_date <= current_date) {
  end_date <- start_date + days(2)
  date_ranges[[length(date_ranges) + 1]] <- list(start = start_date, end = end_date)
  start_date <- start_date + days(3)
}

# Create an empty data frame to store the combined data
combined_data <- data.frame()

# Get the object name for the most recent date
most_recent_date <- yesterday
most_recent_date_end <- data.table::fifelse(date_ranges[[length(date_ranges)]]$end - lubridate::days(2) > most_recent_date,
                               date_ranges[[length(date_ranges) - 1]]$end,
                               date_ranges[[length(date_ranges)]]$end)
most_recent_date_start <- most_recent_date_end - lubridate::days(2)
most_recent_object_name <- paste0("date", format(most_recent_date_start, "%m%d"), format(most_recent_date_end, "%m%d"))

## create dummy data so date07100712 can be rbinded, will be removed as duplicate later
## dates occur during all star break

date07100712 <- date03300401 %>% slice(1)


# Loop through the date ranges and retrieve the data
for (i in seq_along(date_ranges)) {
  start_date <- date_ranges[[i]]$start
  end_date <- date_ranges[[i]]$end
  object_name <- paste0("date", format(start_date, "%m%d"), format(end_date, "%m%d"))
  
  # Check if the data for the date range already exists
  if (exists(object_name)) {
    message("Data for ", object_name, " already exists.")
    # Check if it's the most recent date object and delete it
    if (object_name == most_recent_object_name) {
      rm(list = object_name, envir = .GlobalEnv)
      message("Deleted data for ", object_name)
      data <- statcast_search(start_date = start_date, end_date = end_date, player_type = "batter")
      assign(object_name, data)
    }
    combined_data <- rbind(combined_data, get(object_name))
  }
  
  else{
    data <- statcast_search(start_date = start_date, end_date = end_date, player_type = "batter")
    assign(object_name, data)
    combined_data <- rbind(combined_data, data)
  }
}




## save data under alias to edit and test, remove duplicates as well
use_data <- combined_data[!duplicated(combined_data), ]


#player_table <- chadwick_player_lu()

statcast_ids <- player_table %>% 
  mutate(name = paste(name_first, name_last, sep = " ")) %>% 
  select(name, key_mlbam)

use_data %>% 
  left_join(statcast_ids, by = c("pitcher" = "key_mlbam")) %>% 
  relocate(name) %>% 
  rename("pitcher_name" = name) %>%
  select(-player_name) %>% 
  left_join(statcast_ids, by = c("batter" = "key_mlbam")) %>% 
  rename("batter_name" = name) %>% 
  relocate(batter_name) -> usable_data



## edit copied data here
usable_data %>% code_barrel() %>% 
  mutate(hard_hit = ifelse(launch_speed >= 95, 1, 0),
         sz_left = -0.83,
         sz_right = 0.83,
         actual_strike = ifelse(zone <= 9, 1, 0),
         whiff = ifelse(description == "swinging_strike" | description == "swinging_strike_blocked" | description == "foul_tip", 1, 0),
         swing = ifelse(description != "pitchout" & description != "ball" & description != "called_strike" &  description != "blocked_ball" & description != "hit_by_pitch", 1, 0),
         chase = ifelse(swing == 1 & actual_strike == 0,1,0),
         zone_swing = ifelse(swing == 1 & actual_strike == 1, 1, 0),
         hitting_team = ifelse(inning_topbot == "Top", away_team, home_team),
         pitching_team = ifelse(inning_topbot == "Top", home_team, away_team),
         xwOBA = ifelse(is.na(estimated_woba_using_speedangle), woba_value, estimated_woba_using_speedangle),
         xBA = ifelse(is.na(estimated_ba_using_speedangle), 0, estimated_ba_using_speedangle),
         zone_whiff = ifelse(whiff == 1 & actual_strike == 1, 1, 0),
         runners_on = ifelse(!is.na(on_1b) | !is.na(on_2b) | !is.na(on_3b), 1, 0),
         risp = ifelse(!is.na(on_2b) | !is.na(on_3b), 1, 0),
         spray_angle = round(atan((hc_x-125.42)/(198.27-hc_y))*180/pi*.75, 1),
         field_hit = ifelse(spray_angle < -12.5, "left", ifelse(spray_angle > 12.5, "right", "center")),
         hit_type = case_when(
           field_hit == "left" & stand == "R" ~ "pull",
           field_hit == "center" & stand == "R" ~ "center",
           field_hit == "right" & stand == "R" ~ "oppo",
           field_hit == "left" & stand == "L" ~ "oppo",
           field_hit == "center" & stand == "L" ~ "center",
           field_hit == "right" & stand == "L" ~ "pull"
         ),
         sweet_spot = ifelse(launch_angle >= 8 & launch_angle <= 32, 1, 0),
         pitch_group = case_when(
           pitch_name %in% c("Sweeper", "Curveball", "Slider", "Knucke Curve", "Slurve", "Eephus", "Slow Curve", "Knuckleball") ~ "breaking",
           pitch_name %in% c("4-Seam Fastball", "Sinker", "Cutter") ~ "fastball",
           pitch_name %in% c("Split-Finger", "Forkball", "Screwball", "Changeup") ~ "offspeed"
         )) %>% 
  drop_na(actual_strike) -> data_pitches

## check that data is up to date
print(max(data_pitches$game_date) == Sys.Date()-1) 

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



data_pitches.filter %>% 
  group_by(batter_name) %>% 
  summarise(whiff_rate = round(sum(whiff)/sum(swing), 3),
            o_zone_swing_rate = round(sum(chase, na.rm = T)/sum(actual_strike == 0, na.rm = T),3),
            swing_pct = round(sum(swing)/n(), 3), 
            zone_swing_rate = round(sum(zone_swing, na.rm = T)/sum(actual_strike, na.rm = T),3)) %>% 
  left_join(result_stats)



hits <- c("double", "single", "triple", "home_run")
pa_ab <- c("catcher_interf", "caught_stealing_2b", "caught_stealing_3b",
           "caught_stealing_home", "hit_by_pitch", "pickoff_1b", "pickoff_3b", 
           "pickoff_caught_stealing_2b", "stolen_base_2b", "sac_bunt", "sac_fly",
           "sac_fly_double_play", "wild_pitch", "walk")

filter_statcast_hitter <- function(date = "2023-03-30", pa_limit){
  data_pitches.filter <- data_pitches %>% 
    filter(game_date >= date)
  
  data_results.filter <- data_pitches.filter %>% 
    filter(events != "")
  
  result_stats <- data_results.filter %>% 
    group_by(batter_name) %>% 
    summarise(pa = n(),
              k_rate = sum(events == "strikeout") / n(),
              bb_rate = sum(events == "walk") / n(),
              barrels = sum(barrel, na.rm = TRUE),
              barrel_rate = sum(barrel, na.rm = TRUE) / sum(!is.na(barrel)),
              hard_hit_rate = sum(hard_hit, na.rm = TRUE) / sum(!is.na(hard_hit)),
              hits = sum(events %in% hits),
              ab = sum(!events %in% pa_ab),
              BA = hits / ab,
              wOBA = (0.690 * sum(events == "walk") +
                        0.72 * sum(events == "hit_by_pitch") +
                        .89* sum(events == "single") +
                        1.27 * sum(events == "double") +
                        1.62 * sum(events == "triple") +
                        2.1 * sum(events == "home_run")) / (pa),
              HR = sum(events == "home_run"),
              xwOBA = mean(xwOBA, na.rm = T),
              xBA = mean(xBA, na.rm = T),
              LD_percent = sum(bb_type == "line_drive")/sum(type == "X"),
              GB_percent = sum(bb_type == "ground_ball")/sum(type == "X"),
              FB_percent = sum(bb_type == "fly_ball" | bb_type == "popup")/sum(type == "X"),
              pull_percent = sum(hit_type == "pull", na.rm = T)/sum(type == "X"),
              center_percent = sum(hit_type == "center", na.rm = T)/sum(type == "X"),
              oppo_percent = sum(hit_type == "oppo", na.rm = T)/sum(type == "X"),
              sweet_spot_rate = sum(sweet_spot == 1, na.rm = T)/sum(type == "X")
    )
  
  days_used <- as.numeric(Sys.Date() - as.Date(date))
  
  pa_filter <- ifelse(missing(pa_limit), 2.75*days_used, pa_limit)
  
  print(paste("PA filter used for this data:", pa_filter, sep = " "))
  
  data_pitches.filter %>% 
    group_by(batter_name) %>% 
    summarise(whiff_rate = round(sum(whiff)/sum(swing), 3),
              o_zone_swing_rate = round(sum(chase, na.rm = T)/sum(actual_strike == 0, na.rm = T),3),
              swing_pct = round(sum(swing)/n(), 3), 
              zone_swing_rate = round(sum(zone_swing, na.rm = T)/sum(actual_strike == 1, na.rm = T),3),
              contact_percent = 1-whiff_rate,
              zone_contact_rate = 1 - (round(sum(zone_whiff)/sum(zone_swing), 3))) %>% 
    left_join(result_stats) %>% 
    filter(pa >= pa_filter) %>% 
    return()
}



filter_statcast_hitter(pa_limit = 0) %>% 
  filter(batter_name == "Brett Baty") %>% pull(sweet_spot_rate)


get_rankings_hitter <- function(player_name, Date = "2023-03-30"){
  filtered <- filter_statcast_hitter(date = Date)
  
  if(player_name %in% filtered$batter_name){
    
    data1 <- filtered %>% mutate(
      percentile_whiff = 100 - ntile(whiff_rate, 100),
      percentile_chase = 100 - ntile(o_zone_swing_rate, 100),
      percentile_zone_swing = ntile(zone_swing_rate, 100),
      percentile_zone_contact = ntile(zone_contact_rate, 100),
      percentile_k_rate = 100 - ntile(k_rate, 100),
      percentile_bb_rate = ntile(bb_rate, 100),
      percentile_barrels = ntile(barrels, 100),
      percentile_barrel_rate = ntile(barrel_rate, 100),
      percentile_hard_hit_rate = ntile(hard_hit_rate, 100),
      percentile_BA = ntile(BA, 100),
      percentile_xBA = ntile(xBA, 100),
      percentile_wOBA = ntile(wOBA, 100),
      percentile_xwOBA = ntile(xwOBA, 100),
      percentile_HR = ntile(HR, 100),
      percentile_GB = ntile(GB_percent, 100),
      percentile_LD = ntile(LD_percent, 100), 
      percentile_FB = ntile(FB_percent, 100)
    ) %>%   
      filter(batter_name == player_name) %>% 
      pivot_longer(
        cols = starts_with("percentile_"),
        names_to = "variable",
        values_to = "percentile_value",
        names_prefix = "percentile_"
      ) %>% select(variable, percentile_value)
    
    data2 <- filtered %>% 
      select(batter_name, whiff_rate, o_zone_swing_rate, zone_swing_rate, zone_contact_rate, 
             k_rate, bb_rate, barrels, barrel_rate, hard_hit_rate, BA, xBA, wOBA, xwOBA, HR, GB_percent,
             LD_percent, FB_percent) %>% 
      filter(batter_name == player_name) %>% 
      pivot_longer(cols = whiff_rate:FB_percent,
                   names_to = "variable",
                   values_to = "stat_values")%>% select(stat_values)
    
    cbind(data1, data2) %>% tibble() %>% return() }
  
  else{
    print("This player did not qualify for plate appearances in this timespan. Using no PA filter")
    
    filtered <- filter_statcast_hitter(date = Date, pa = 0)
    
    data1 <- filtered %>% 
      select(batter_name, whiff_rate, o_zone_swing_rate, zone_swing_rate, zone_contact_rate, 
             k_rate, bb_rate, barrels, barrel_rate, hard_hit_rate, BA, xBA, wOBA, xwOBA, HR, GB_percent,
             LD_percent, FB_percent) %>% mutate(
               percentile_whiff = 100 - ntile(whiff_rate, 100),
               percentile_chase = 100 - ntile(o_zone_swing_rate, 100),
               percentile_zone_swing = ntile(zone_swing_rate, 100),
               percentile_zone_contact = ntile(zone_contact_rate, 100),
               percentile_k_rate = 100 - ntile(k_rate, 100),
               percentile_bb_rate = ntile(bb_rate, 100),
               percentile_barrels = ntile(barrels, 100),
               percentile_barrel_rate = ntile(barrel_rate, 100),
               percentile_hard_hit_rate = ntile(hard_hit_rate, 100),
               percentile_BA = ntile(BA, 100),
               percentile_xBA = ntile(xBA, 100),
               percentile_wOBA = ntile(wOBA, 100),
               percentile_xwOBA = ntile(xwOBA, 100),
               percentile_HR = ntile(HR, 100),
               percentile_GB = ntile(GB_percent, 100),
               percentile_LD = ntile(LD_percent, 100), 
               percentile_FB = ntile(FB_percent, 100)
             ) %>%   
      filter(batter_name == player_name) %>% 
      pivot_longer(
        cols = starts_with("percentile_"),
        names_to = "variable",
        values_to = "percentile_value",
        names_prefix = "percentile_"
      ) %>% select(variable, percentile_value)
    
    data2 <- filtered %>% 
      select(batter_name, whiff_rate, o_zone_swing_rate, zone_swing_rate, zone_contact_rate, 
             k_rate, bb_rate, barrels, barrel_rate, hard_hit_rate, BA, xBA, wOBA, xwOBA, HR, GB_percent,
             LD_percent, FB_percent) %>% 
      filter(batter_name == player_name) %>% 
      pivot_longer(cols = whiff_rate:FB_percent,
                   names_to = "variable",
                   values_to = "stat_values") %>% select(stat_values)
    
    
    cbind(data1, data2) %>% tibble() %>% return()
  }
}


player <- "Joc Pederson";get_rankings_hitter(player, Sys.Date() - 30);get_plots(player, "PTS")



get_rankings_pitcher("Tanner Bibee", Sys.Date()-30)


get_moving_averages("PTS") %>% view()


filter_statcast_pitcher <- function(date = "2023-03-30", pa_limit){
  data_pitches.filter <- data_pitches %>% 
    filter(game_date >= date)
  
  data_results.filter <- data_pitches.filter %>% 
    filter(events != "")
  
  result_stats <- data_results.filter %>% 
    group_by(pitcher_name) %>% 
    summarise(pa = n(),
              k_rate = sum(events == "strikeout") / n(),
              bb_rate = sum(events == "walk") / n(),
              barrels = sum(barrel, na.rm = TRUE),
              barrel_rate = sum(barrel, na.rm = TRUE) / sum(!is.na(barrel)),
              hard_hit_rate = sum(hard_hit, na.rm = TRUE) / sum(!is.na(hard_hit)),
              hits = sum(events %in% hits),
              ab = sum(!events %in% pa_ab),
              BA = hits / ab,
              wOBA = (0.690 * sum(events == "walk") +
                        0.72 * sum(events == "hit_by_pitch") +
                        .89* sum(events == "single") +
                        1.27 * sum(events == "double") +
                        1.62 * sum(events == "triple") +
                        2.1 * sum(events == "home_run")) / (pa),
              HR = sum(events == "home_run"),
              xwOBA = mean(xwOBA, na.rm = T),
              xBA = mean(xBA, na.rm = T),
              LD_percent = sum(bb_type == "line_drive")/n(),
              GB_percent = sum(bb_type == "ground_ball")/n(),
              FB_percent = sum(bb_type == "fly_ball" | bb_type == "popup")/n()
    )
  
  days_used <- as.numeric(Sys.Date() - as.Date(date))
  
  pa_filter <- ifelse(missing(pa_limit), 2.75*days_used, pa_limit)
  
  print(paste("PA filter used for this data:", pa_filter, sep = " "))
  
  data_pitches.filter %>% 
    group_by(pitcher_name) %>% 
    summarise(whiff_rate = round(sum(whiff)/sum(swing), 3),
              o_zone_swing_rate = round(sum(chase, na.rm = T)/sum(actual_strike == 0, na.rm = T),3),
              swing_pct = round(sum(swing)/n(), 3), 
              zone_swing_rate = round(sum(zone_swing, na.rm = T)/sum(actual_strike, na.rm = T),3),
              contact_percent = 1-whiff_rate) %>% 
    left_join(result_stats) %>% 
    filter(pa >= pa_filter) %>% 
    return()
}


filter_statcast_pitcher(Sys.Date() - 14) %>% 
  arrange((xwOBA)) %>% 
  print(n = 20)


get_rankings_pitcher <- function(player, date = "2023-03-30"){
  filtered <- filter_statcast_pitcher(date = date)
  
  filtered <- filtered %>% 
    mutate(
      percentile_k_rate = ntile(k_rate, 100),
      percentile_bb_rate = 100 - ntile(bb_rate, 100),
      percentile_barrels = 100 - ntile(barrels, 100),
      percentile_barrel_rate = 100 - ntile(barrel_rate, 100),
      percentile_hard_hit_rate = 100 - ntile(hard_hit_rate, 100),
      percentile_hits = 100 - ntile(hits, 100),
      percentile_ab = ntile(ab, 100),
      percentile_BA = 100 - ntile(BA, 100),
      percentile_xBA = 100 - ntile(xBA, 100),
      percentile_wOBA = 100 - ntile(wOBA, 100),
      percentile_xwOBA = 100 - ntile(xwOBA, 100),
      percentile_HR = 100 - ntile(HR, 100),
      percentile_whiff = ntile(whiff_rate, 100),
      percentile_chase = ntile(o_zone_swing_rate, 100)
    )
  
  if(player %in% filtered$pitcher_name){
    
    return(filtered %>% 
             filter(pitcher_name == player) %>% 
             select(
               pitcher_name,
               percentile_k_rate,
               percentile_bb_rate,
               percentile_barrels,
               percentile_barrel_rate,
               percentile_hard_hit_rate,
               percentile_hits,
               percentile_ab,
               percentile_BA,
               percentile_xBA,
               percentile_wOBA,
               percentile_xwOBA,
               percentile_HR,
               percentile_whiff,
               percentile_chase
             ) %>% 
             pivot_longer(
               cols = starts_with("percentile_"),
               names_to = "variable",
               values_to = "percentile_value",
               names_prefix = "percentile_"
             ))}
  
  else{
    print("This player did not qualify for plate appearances in this timespan. Using no PA filter")
    
    filtered <- filter_statcast_pitcher(date = date, pa = 0)
    
    filtered <- filtered %>% 
      mutate(
        percentile_k_rate = ntile(k_rate, 100),
        percentile_bb_rate = 100 - ntile(bb_rate, 100),
        percentile_barrels = 100 - ntile(barrels, 100),
        percentile_barrel_rate = 100 - ntile(barrel_rate, 100),
        percentile_hard_hit_rate = 100 - ntile(hard_hit_rate, 100),
        percentile_hits = 100 - ntile(hits, 100),
        percentile_ab = ntile(ab, 100),
        percentile_BA = 100 - ntile(BA, 100),
        percentile_xBA = 100 - ntile(xBA, 100),
        percentile_wOBA = 100 - ntile(wOBA, 100),
        percentile_xwOBA = 100 - ntile(xwOBA, 100),
        percentile_HR = 100 - ntile(HR, 100),
        percentile_whiff = ntile(whiff_rate, 100),
        percentile_chase = ntile(o_zone_swing_rate, 100)
      )
    
    return(filtered %>% 
             filter(pitcher_name == player) %>% 
             select(
               pitcher_name,
               percentile_k_rate,
               percentile_bb_rate,
               percentile_barrels,
               percentile_barrel_rate,
               percentile_hard_hit_rate,
               percentile_hits,
               percentile_ab,
               percentile_BA,
               percentile_xBA,
               percentile_wOBA,
               percentile_xwOBA,
               percentile_HR,
               percentile_whiff,
               percentile_chase
             ) %>% 
             pivot_longer(
               cols = starts_with("percentile_"),
               names_to = "variable",
               values_to = "percentile_value",
               names_prefix = "percentile_"
             ))
    
    
  }
}


get_rankings_pitcher("Blake Snell", Sys.Date() - 30)






### statcast pitching data by teams
filter_statcast_team_pitching <- function(date = "2023-03-30"){
  data_pitches.filter <- data_pitches %>% 
    filter(game_date >= date)
  
  data_results.filter <- data_pitches.filter %>% 
    filter(events != "")
  
  result_stats <- data_results.filter %>% 
    group_by(pitching_team) %>% 
    summarise(pa = n(),
              k_rate = sum(events == "strikeout") / n(),
              bb_rate = sum(events == "walk") / n(),
              barrels = sum(barrel, na.rm = TRUE),
              barrel_rate = sum(barrel, na.rm = TRUE) / sum(!is.na(barrel)),
              hard_hit_rate = sum(hard_hit, na.rm = TRUE) / sum(!is.na(hard_hit)),
              hits = sum(events %in% hits),
              ab = sum(!events %in% pa_ab),
              BA = hits / ab,
              wOBA = (0.690 * sum(events == "walk") +
                        0.72 * sum(events == "hit_by_pitch") +
                        .89* sum(events == "single") +
                        1.27 * sum(events == "double") +
                        1.62 * sum(events == "triple") +
                        2.1 * sum(events == "home_run")) / (pa),
              HR = sum(events == "home_run"),
              xwOBA = mean(xwOBA, na.rm = T),
              xBA = mean(xBA, na.rm = T)
    )
  
  days_used <- as.numeric(Sys.Date() - as.Date(date))
  
  data_pitches.filter %>% 
    group_by(pitching_team) %>% 
    summarise(whiff_rate = round(sum(whiff)/sum(swing), 3),
              o_zone_swing_rate = round(sum(chase, na.rm = T)/sum(actual_strike == 0, na.rm = T),3),
              swing_pct = round(sum(swing)/n(), 3), 
              zone_swing_rate = round(sum(zone_swing, na.rm = T)/sum(actual_strike == 1, na.rm = T),3),
              contact_percent = 1-whiff_rate) %>% 
    left_join(result_stats) %>%
    return()
}


filter_statcast_team_pitching(Sys.Date() - 21) %>% 
  print(n = 30)


## statcast hitting data by teams
filter_statcast_team_hitting <- function(date = "2023-03-30", hand_fill = "none", risp = FALSE){
  if(risp == TRUE){
    data_pitches <- data_pitches %>% 
      filter(risp == 1)
  }
  if(tolower(hand_fill) == "l"){
    data_pitches.filter <- data_pitches %>% 
      filter(game_date >= date, p_throws == "L")
  }
  if(tolower(hand_fill) == "r"){
    data_pitches.filter <- data_pitches %>% 
      filter(game_date >= date, p_throws == "R")
  }
  else{
    data_pitches.filter <- data_pitches %>% 
      filter(game_date >= date) 
  }
  
  data_results.filter <- data_pitches.filter %>% 
    filter(events != "")
  
  result_stats <- data_results.filter %>% 
    group_by(hitting_team) %>% 
    summarise(pa = n(),
              k_rate = sum(events == "strikeout") / n(),
              bb_rate = sum(events == "walk") / n(),
              barrels = sum(barrel, na.rm = TRUE),
              barrel_rate = sum(barrel, na.rm = TRUE) / sum(!is.na(barrel)),
              hard_hit_rate = sum(hard_hit, na.rm = TRUE) / sum(!is.na(hard_hit)),
              hits = sum(events %in% hits),
              ab = sum(!events %in% pa_ab),
              BA = hits / ab,
              wOBA = (0.690 * sum(events == "walk") +
                        0.72 * sum(events == "hit_by_pitch") +
                        .89* sum(events == "single") +
                        1.27 * sum(events == "double") +
                        1.62 * sum(events == "triple") +
                        2.1 * sum(events == "home_run")) / (pa),
              woba2 = sum(woba_value, na.rm = T)/sum(woba_denom, na.rm = T),
              HR = sum(events == "home_run"),
              xwOBA = mean(xwOBA, na.rm = T),
              xBA = mean(xBA, na.rm = T)
    )
  
  days_used <- as.numeric(Sys.Date() - as.Date(date))
  
  data_pitches.filter %>% 
    group_by(hitting_team) %>% 
    summarise(whiff_rate = round(sum(whiff)/sum(swing), 3),
              o_zone_swing_rate = round(sum(chase, na.rm = T)/sum(actual_strike == 0, na.rm = T),3),
              swing_pct = round(sum(swing)/n(), 3), 
              zone_swing_rate = round(sum(zone_swing, na.rm = T)/sum(actual_strike, na.rm = T),3),
              contact_percent = 1-whiff_rate) %>% 
    left_join(result_stats) %>%
    return()
}

filter_statcast_team_hitting(Sys.Date() - 30, hand_fill = "l") %>% 
  arrange(desc(bb_rate)) %>% 
  relocate(hitting_team, wOBA, xwOBA) %>% 
  print(n = 30)



filter_statcast_hitter() %>% 
  arrange(desc(xwOBA))


get_rankings_pitcher("Griffin Canning")

get_rankings_hitter("Wil Myers")





## save data
write_rds(data_pitches, "statcast2023.rds")

### hitting vs lefties
write_rds(filter_statcast_team_hitting(Sys.Date() - 30, hand_fill = "L"), "predictors/lefties.rds")

### hitting vs righties
write_rds(filter_statcast_team_hitting(Sys.Date() - 21, hand_fill = "R"), "predictors/righties.rds")



## function to filter on a per-game level
filter_statcast_hitter_date <- function(date = "2023-03-30", pa_limit){
  data_pitches.filter <- data_pitches %>% 
    filter(game_date >= date)
  
  data_results.filter <- data_pitches.filter %>% 
    filter(events != "")
  
  result_stats <- data_results.filter %>% 
    group_by(batter_name, game_date) %>% 
    summarise(pa = n(),
              k_rate = sum(events == "strikeout") / n(),
              bb_rate = sum(events == "walk") / n(),
              barrels = sum(barrel, na.rm = TRUE),
              barrel_rate = sum(barrel, na.rm = TRUE) / sum(!is.na(barrel)),
              hard_hit_rate = sum(hard_hit, na.rm = TRUE) / sum(!is.na(hard_hit)),
              hits = sum(events %in% hits),
              ab = sum(!events %in% pa_ab),
              BA = hits / ab,
              singles = sum(events == "single"),
              doubles = sum(events == "double"),
              triples = sum(events == "triple"),
              home_runs = sum(events == "home_run"),
              walks = sum(events == "walk" | events == "hit_by_pitch"),
              strikeouts = sum(events == "strikeout"),
              PTS = 1.5 * singles + 3 * doubles + 4.5 * triples + 6 * home_runs + 1.5 * walks - 1.5 * strikeouts,
              wOBA = (0.690 * sum(events == "walk") +
                        0.72 * sum(events == "hit_by_pitch") +
                        .89* sum(events == "single") +
                        1.27 * sum(events == "double") +
                        1.62 * sum(events == "triple") +
                        2.1 * sum(events == "home_run")) / (pa),
              HR = sum(events == "home_run"),
              xwOBA = mean(xwOBA, na.rm = T),
              xBA = mean(estimated_ba_using_speedangle, na.rm = T)
    )
  
  days_used <- as.numeric(Sys.Date() - as.Date(date))
  
  pa_filter <- ifelse(missing(pa_limit), 2.75*days_used, pa_limit)
  
  print(paste("PA filter used for this data:", pa_filter, sep = " "))
  
  data_pitches.filter %>% 
    group_by(batter_name, game_date) %>% 
    summarise(whiff_rate = round(sum(whiff)/sum(swing), 3),
              o_zone_swing_rate = round(sum(chase, na.rm = T)/sum(actual_strike == 0, na.rm = T),3),
              swing_pct = round(sum(swing)/n(), 3), 
              zone_swing_rate = round(sum(zone_swing, na.rm = T)/sum(swing, na.rm = T),3),
              contact_percent = 1-whiff_rate) %>% 
    left_join(result_stats) %>%
    return()
}

statcast_gamelogs <- filter_statcast_hitter_date(pa_limit = 0)





### get moving averages (sortable)
get_moving_averages <- function(stat){
  statcast_gamelogs %>% 
    mutate(
      PTS = 1.5 * singles + 3 * doubles + 4.5 * triples + 6 * home_runs + 1.5 * walks - 1.5 * strikeouts,
      tb = singles + 2 * doubles + 3 * triples + 4 * home_runs,
      five_game = (zoo::rollmean(!!sym(stat)*(1-k_rate)*(1+bb_rate), k = 5, align = "right", fill = NA)),
      ten_game = (zoo::rollmean(!!sym(stat)*(1-k_rate)*(1+bb_rate), k = 10, align = "right", fill = NA)),
      three_game = (zoo::rollmean(!!sym(stat)*(1-k_rate)*(1+bb_rate), k = 3, align = "right", fill = NA)),
      fifteen_game = (zoo::rollmean(!!sym(stat)*(1-k_rate)*(1+bb_rate), k = 15, align = "right", fill = NA))
    ) %>% 
    arrange(desc(game_date)) %>% 
    relocate(fifteen_game) %>% 
    distinct(batter_name, .keep_all = TRUE)
}


### moving average plotting

get_moving_averages2 <- function(stat) {
  statcast_gamelogs %>%
    mutate(
      PTS = 1.5 * singles + 3 * doubles + 4.5 * triples + 6 * home_runs + 1.5 * walks - 1.5 * strikeouts,
      tb = singles + 2 * doubles + 3 * triples + 4 * home_runs,
      five_game = zoo::rollmean(!!sym(stat) * (1 - k_rate) * (1 + bb_rate), k = 5, align = "right", fill = NA),
      ten_game = zoo::rollmean(!!sym(stat) * (1 - k_rate) * (1 + bb_rate), k = 10, align = "right", fill = NA),
      three_game = zoo::rollmean(!!sym(stat) * (1 - k_rate) * (1 + bb_rate), k = 3, align = "right", fill = NA),
      fifteen_game = zoo::rollmean(!!sym(stat) * (1 - k_rate) * (1 + bb_rate), k = 15, align = "right", fill = NA)
    ) %>%
    arrange(desc(game_date)) %>%
    relocate(fifteen_game)
}

plot_player_averages <- function(filtered_data, player_name) {
  ggplot(filtered_data, aes(x = game_date)) +
    geom_line(aes(y = five_game, color = "5-game average")) +
    geom_line(aes(y = ten_game, color = "10-game average")) +
    geom_line(aes(y = three_game, color = "3-game average")) +
    geom_line(aes(y = fifteen_game, color = "15-game average")) +
    labs(x = "Game Date", y = "Moving Average", color = "Moving Average") +
    scale_color_manual(values = c("5-game average" = "red", "10-game average" = "blue",
                                  "3-game average" = "green", "15-game average" = "purple"),
                       labels = c("5-game average", "10-game average",
                                  "3-game average", "15-game average")) +
    ggtitle(paste("Moving Averages for", player_name))
}

filter_and_plot_averages <- function(player_name, stat) {
  filtered_data <- get_moving_averages2(stat) %>%
    filter(batter_name == player_name)
  
  plot_player_averages(filtered_data, player_name)
}

# Example usage
filter_and_plot_averages("Maikel Garcia", "PTS")




