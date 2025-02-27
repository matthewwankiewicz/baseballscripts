### load packages
library(baseballr)
library(lubridate)
library(tidyverse)
library(job)

## scrape data
data_pitches <- read_rds("data_folder/data_pitches.rds")

# Get today's date and yesterday's date
current_date <- Sys.Date() 
yesterday <- current_date - days(1)

# Define the start date as March 30th or the next day after the last saved data
start_date <- max(data_pitches$game_date)

# Create an empty list to store the date ranges
date_ranges <- list()

# Loop to generate date ranges from the start date to the current date
while (start_date <= current_date) {
  end_date <- start_date + days(2)
  date_ranges[[length(date_ranges) + 1]] <- list(start = start_date, end = end_date)
  start_date <- start_date + days(3)
}

# Create an empty data frame to store the combined data
combined_data <- list()

# Get the object name for the most recent date
most_recent_date <- yesterday
most_recent_date_end <- data.table::fifelse(date_ranges[[length(date_ranges)]]$end - lubridate::days(2) > most_recent_date,
                                            date_ranges[[length(date_ranges)]]$end - 1,
                                            date_ranges[[length(date_ranges)]]$end)
most_recent_date_start <- most_recent_date_end - lubridate::days(2)
most_recent_object_name <- paste0("date", format(most_recent_date_start, "%m%d"), format(most_recent_date_end, "%m%d"))

# Loop through the date ranges and retrieve the data
### use job function to run in backgound


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
      data <- baseballr::statcast_search(start_date = start_date, end_date = end_date)
      assign(object_name, data)
    }
    combined_data[[i]] <- get(object_name) %>% mutate(game_date = as.Date(game_date))
  }
  
  else{
    data <- statcast_search(start_date = start_date, end_date = end_date, player_type = "batter")
    assign(object_name, data)
    combined_data[[i]] <- data %>% 
      mutate(game_date = as.Date(game_date))
  }
}

### combine the list into one table
pitching_data_new <- do.call(rbind, combined_data)

data <- statcast_search(start_date = "2024-04-21", end_date = "2024-04-21", player_type = "batter")

## save data under alias to edit and test, remove duplicates as well
use_data <- pitching_data_new[!duplicated(pitching_data_new), ]


## get player id table
player_table <- chadwick_player_lu()

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
         ),
         pfx_x_in_pv = -12*pfx_x,
         pfx_z_in = 12*pfx_z,
         season = substr(game_date, 1, 4)) %>% 
  drop_na(actual_strike) -> data_pitches_new

## check that data is up to date
print(max(data_pitches_new$game_date) == Sys.Date()-1) 

## combine older and new data
data_pitches <- rbind(data_pitches, data_pitches_new, fill = TRUE)

## remove any duplicates
data_pitches <- data_pitches[!duplicated(data_pitches)]

## save file
write_rds(data_pitches, "data_folder/data_pitches.rds")

data_pitches_new <- data_pitches_new %>% 
  distinct(pitcher_name, batter_name, release_pos_x, release_pos_z, barrel, 
           pitch_number, pitch_name, game_date, release_speed, 
           release_spin_rate, .keep_all = T)
dbWriteTable(con, "rawstatcast", value = data_pitches_new, row.names = FALSE,
             overwrite = F, append = T)



pitcher_daily_stats <- data_pitches %>% 
  distinct(pitch_name, pitcher_name, game_date, outs_when_up, 
           release_pos_x, release_pos_z, release_spin_rate, .keep_all = T) %>% 
  group_by(pitcher_name, game_date, hitting_team) %>% 
  summarise(k = sum(events == "strikeout"),
            bb = sum(events == "walk"),
            outs = sum(grepl("out", des) & events != ""))


## write to database
dbWriteTable(con,  "pitcherdailystats", pitcher_daily_stats)




dbGetQuery(con, 
           "SELECT
    pitcher_name,
    game_date,
    hitting_team,
    SUM(CASE WHEN events = 'strikeout' THEN 1 ELSE 0 END) AS k,
    SUM(CASE WHEN events = 'walk' THEN 1 ELSE 0 END) AS bb,
    SUM(CASE WHEN des LIKE '%out%' AND events != '' THEN 1 ELSE 0 END) AS outs
FROM
    (SELECT DISTINCT ON (pitch_name, pitcher_name, game_date, outs_when_up, release_pos_x, release_pos_z, release_spin_rate)
        *
     FROM
        rawstatcast) AS distinct_pitches
GROUP BY
    pitcher_name,
    game_date,
    hitting_team;
")


dbSendQuery(con, "create table pitcherdailystats as
            SELECT
    pitcher_name,
    game_date,
    hitting_team,
    SUM(CASE WHEN events = 'strikeout' THEN 1 ELSE 0 END) AS k,
    SUM(CASE WHEN events = 'walk' THEN 1 ELSE 0 END) AS bb,
    SUM(CASE WHEN des LIKE '%out%' AND events != '' THEN 1 ELSE 0 END) AS outs
FROM
    (SELECT DISTINCT ON (pitch_name, pitcher_name, game_date, outs_when_up, release_pos_x, release_pos_z, release_spin_rate)
        *
     FROM
        rawstatcast) AS distinct_pitches
GROUP BY
    pitcher_name,
    game_date,
    hitting_team;
")


dbGetQuery(con, "SELECT s.*, d.*,
       CASE
           WHEN d.k > s.line THEN 'True'
           ELSE 'False'
       END AS over
FROM pitcherstrikeoutprops s
JOIN pitcherdailystats d ON 
     (s.full_name = d.pitcher_name AND
      s.bet_date = d.game_date);
") %>% View

## table for overs
dbSendQuery(con, "create table pitcheroverprops as
           select * from pitcherstrikeoutprops where ou = 'over';")

## table for unders
dbSendQuery(con, "create table pitcherunderprops as
           select * from pitcherstrikeoutprops where ou = 'under';")



dbGetQuery(con, "SELECT s.*, d.*,
       CASE
           WHEN d.k > s.line THEN 1
           ELSE 0
       END AS over
FROM pitcheroverprops s
JOIN pitcherdailystats d ON 
     (s.full_name = d.pitcher_name AND
      s.bet_date = d.game_date);
")


dbGetQuery(con, "select avg(outs) from pitcherdailystats where
           pitcher_name in (select pitcher_name from pitcherdailystats where
           hitting_team = 'TOR' and game_date >= '2024-03-28') and pitcher_name in 
           (select full_name from pitcheroverprops) and
           game_date >= '2024-03-28' and hitting_team != 'TOR';")


dbGetQuery(con, "select * from pitcheroverprops")