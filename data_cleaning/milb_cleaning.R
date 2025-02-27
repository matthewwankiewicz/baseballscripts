library(tidyverse)
library(baseballr)
library(data.table)

aaa_pitch_data <- readRDS("aaa_pitch_data.rds")



# Create a named vector to map old column names to new ones
rename_map <- c(
  "matchup.batter.fullName" = "batter_name",
  "matchup.pitcher.fullName" = "pitcher_name",
  "details.type.code" = "pitch_type",
  "game_date" = "game_date",
  "pitchData.startSpeed" = "release_speed",
  "pitchData.coordinates.x0" = "release_pos_x",
  "pitchData.coordinates.z0" = "release_pos_z",
  "matchup.batter.id" = "batter",
  "matchup.pitcher.id" = "pitcher",
  "result.event" = "events",
  "details.description" = "description",
  "hitData.launchAngle" = "launch_angle",
  "hitData.launchSpeed" = "launch_speed",
  "pitchData.breaks.spinDirection" = "spin_dir",
  "pitchData.breaks.spinRate" = "spin_rate_deprecated",
  "pitchData.breaks.breakAngle" = "break_angle_deprecated",
  "pitchData.breaks.breakLength" = "break_length_deprecated",
  "pitchData.zone" = "zone",
  "result.description" = "des",
  "matchup.batSide.code" = "stand",
  "matchup.pitchHand.code" = "p_throws",
  "home_team" = "home_team",
  "away_team" = "away_team",
  "details.type.description" = "pitch_name",
  "hitData.location" = "hit_location",
  "hitData.trajectory" = "bb_type",
  "count.balls.start" = "balls",
  "count.strikes.start" = "strikes",
  "about.inning" = "inning",
  "count.outs.start" = "outs_when_up",
  "pitchData.coordinates.pfxX" = "pfx_x",
  "pitchData.coordinates.pfxZ" = "pfx_z",
  "pitchData.coordinates.pX" = "plate_x",
  "pitchData.coordinates.pZ" = "plate_z",
  "matchup.postOnThird.id" = "on_3b",
  "matchup.postOnSecond.id" = "on_2b",
  "matchup.postOnFirst.id" = "on_1b",
  "hitData.coordinates.coordX" = "hc_x",
  "hitData.coordinates.coordY" = "hc_y",
  "pitchData.coordinates.aY" = "ay",
  "pitchData.coordinates.aZ" = "az",
  "pitchData.coordinates.y0" = "y0",
  "pitchData.coordinates.vY0" = "vy0",
  "pitchData.coordinates.vX0" = "vx0",
  "pitchData.coordinates.vZ0" = "vz0",
  "pitchData.extension" = "release_extension"
)

# Example dataframe (replace with your actual dataset)
aaa_pitch_data_clean <- data.table::data.table(aaa_pitch_data)


setnames(aaa_pitch_data_clean, old = names(rename_map),
         new = rename_map)


aaa_pitch_data_clean <- aaa_pitch_data_clean %>% 
  code_barrel() %>% 
  mutate(hard_hit = ifelse(launch_speed >= 95, 1, 0),
         sz_left = -0.83,
         sz_right = 0.83,
         actual_strike = ifelse(zone <= 9, 1, 0),
         whiff = ifelse(details.call.description %in% c("Foul Tip", "Swinging Strike", "Swinging Strike (Blocked)"), 1, 0),
         swing = ifelse(grepl('swing|foul', details.call.description, ignore.case = T)|details.call.code=='X', 1, 0),
         chase = ifelse(swing == 1 & actual_strike == 0,1,0),
         zone_swing = ifelse(swing == 1 & actual_strike == 1, 1, 0),
         hitting_team = ifelse(about.isTopInning, away_team, home_team),
         pitching_team = ifelse(about.isTopInning, home_team, away_team),
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
           pitch_name %in% c("Four-Seam Fastball", "Sinker", "Cutter") ~ "fastball",
           pitch_name %in% c("Split-Finger", "Forkball", "Screwball", "Changeup") ~ "offspeed"
         ),
         ext_velocity = sqrt(vx0^2 + vy0^2 + vz0^2),  # Total release velocity
         perceived_velocity = ext_velocity * (60.5 / (60.5 - release_extension)),
         expected_chase = case_when(
           pitch_group == "breaking" & actual_strike == 0 ~ 0.45,  # Sliders outside
           pitch_group == "fastball" & actual_strike == 0 ~ 0.35,  # Fastballs high
           pitch_group == "offspeed" & actual_strike == 0 ~ 0.40,  # Changeups low
           TRUE ~ 0.30
         ),
         edge = ifelse(zone %in% c(11, 12, 13, 14, 16, 17, 18, 19), 1, 0)) %>% 
  drop_na(actual_strike) 



aaa_pitch_data_clean %>% 
  group_by(pitcher_name) %>% 
  filter(n()<1500 & n()>100) %>% 
  summarise(whiff_rate = mean(whiff[swing==T], na.rm=T),
            barrel_rate = mean(barrel[bb_type!='' & description!='foul'], na.rm=T),
            exit_velo = mean(launch_speed[bb_type!='' & description!='foul'], na.rm=T),
            fastball_velo = mean(release_speed[pitch_group == "fastball"], na.rm=T),
            sweet_spot_rate = mean(sweet_spot[bb_type!='' & description!='foul'], na.rm = T),
            k_rate = mean(result.eventType[last.pitch.of.ab=="true"] == "strikeout", na.rm=T),
            bb_rate = mean(result.eventType[last.pitch.of.ab=="true"] == "walk", na.rm=T),
            hard_hit_rate = mean(hard_hit[bb_type != ''], na.rm=T),
            groundball_rate = mean(bb_type[bb_type!=''] == "ground_ball", na.rm = T),
            extension = mean(release_extension, na.rm=T),
            edge_rate = mean(edge, na.rm=T),
            expected_chase = mean(expected_chase, na.rm = T),
            chase_rate = mean(chase[actual_strike==F], na.rm=T),
            strike_pct = mean(actual_strike, na.rm=T)) 


### aa data
aa_pitch_data <- readRDS("aa_pitch_data.rds")


### clean
# Example dataframe (replace with your actual dataset)
aa_pitch_data_clean <- data.table::data.table(aa_pitch_data)


setnames(aa_pitch_data_clean, old = names(rename_map),
         new = rename_map)


aa_pitch_data_clean <- aa_pitch_data_clean %>% 
  code_barrel() %>%
  mutate(hard_hit = ifelse(launch_speed >= 95, 1, 0),
         sz_left = -0.83,
         sz_right = 0.83,
         actual_strike = ifelse(zone <= 9, 1, 0),
         whiff = ifelse(details.call.description %in% c("Foul Tip", "Swinging Strike", "Swinging Strike (Blocked)"), 1, 0),
         swing = ifelse(grepl('swing|foul', details.call.description, ignore.case = T), 1, 0),
         chase = ifelse(swing == 1 & actual_strike == 0,1,0),
         zone_swing = ifelse(swing == 1 & actual_strike == 1, 1, 0),
         hitting_team = ifelse(about.isTopInning, away_team, home_team),
         pitching_team = ifelse(about.isTopInning, home_team, away_team),
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
           pitch_name %in% c("Four-Seam Fastball", "Sinker", "Cutter") ~ "fastball",
           pitch_name %in% c("Split-Finger", "Forkball", "Screwball", "Changeup") ~ "offspeed"
         ),
         ext_velocity = sqrt(vx0^2 + vy0^2 + vz0^2),  # Total release velocity
         perceived_velocity = ext_velocity * (60.5 / (60.5 - release_extension)),
         expected_chase = case_when(
           pitch_group == "breaking" & actual_strike == 0 ~ 0.45,  # Sliders outside
           pitch_group == "fastball" & actual_strike == 0 ~ 0.35,  # Fastballs high
           pitch_group == "offspeed" & actual_strike == 0 ~ 0.40,  # Changeups low
           TRUE ~ 0.30
         ),
         edge = ifelse(zone %in% c(11, 12, 13, 14, 16, 17, 18, 19), 1, 0))

### assign level
aa_pitch_data_clean$level <- "aa"
aaa_pitch_data_clean$level <- "aaa"

milb_pitch_data <- rbind(aa_pitch_data_clean, aaa_pitch_data_clean, fill=TRUE)

saveRDS(milb_pitch_data, "milb_pitch_data.rds")
