data_pitches %>% 
  group_by(pitcher_name, pitch_name) %>% 
  summarise(avg_velo = mean(release_speed, na.rm = T),
            avg_h_break = mean(pfx_x, na.rm = T),
            avg_v_break = mean(pfx_z, na.rm = T)) %>% 
  filter(pitch_name %in% c("4-Seam Fastball", "Sinker")) -> velo_key

data_pitches %>% 
  group_by(pitcher_name, pitch_name) %>% 
  summarise(avg_velo = mean(release_speed, na.rm = T),
            avg_h_break = mean(pfx_x, na.rm = T),
            avg_v_break = mean(pfx_z, na.rm = T)) %>% 
  filter(pitch_name %in% c("Split-Finger", "Forkball", "Screwball", "Changeup")) -> velo_off_key




### fastball stuff
whiff_data_f <- data_pitches %>% 
  mutate(desired_outcome = ifelse(description %in% c("called_strike", "swinging_strike", "foul_tip",
                                                     "swinging_strike_blocked", "missed_bunt"), 1, 0),
         pfx_x = pfx_x*-12,
         pfx_z = pfx_z*12,
         total_break = sqrt(pfx_x**2 + pfx_z**2)) %>% 
  filter(pitch_name != "" | pitch_name != "Pitch Out",
         pitch_group == "fastball") %>% 
  left_join(velo_off_key, by = c("pitcher_name")) %>% 
  select(-c(pitch_name.y)) %>% 
  rename("pitch_name" = pitch_name.x) %>% 
  mutate(velo_diff = release_speed - avg_velo,
         h_break_diff = pfx_x - avg_h_break,
         v_break_diff = pfx_z - avg_v_break,
         vy_f = -sqrt(vy0**2 - (2*ay*(50-17/12))),
         t = (vy_f - vy0)/ay,
         vz_f = vz0 + (az*t),
         VAA = -atan(vz_f/vy_f)*(180/pi)) %>% 
  filter(swing == 1)


set.seed(21)
dt = sort(sample(nrow(whiff_data_f), nrow(whiff_data_f)*.7))
train<-whiff_data_f[dt,]
test<-whiff_data_f[-dt,]
test <- test %>%
  drop_na(release_speed, release_pos_x, release_pos_z, pfx_x, pfx_z)

whiff.mod.f <- glm(whiff ~ total_break + release_speed:velo_diff + release_spin_rate + h_break_diff + v_break_diff + VAA,
                   data = train %>% filter(pitch_group == "fastball"), family = binomial())


summary(whiff.mod.f)

test <- test %>% 
  drop_na(pfx_z, pitch_group)


test$whiff_plus <- predict(whiff.mod.f, newdata = test, type = "response")*(100/predict(whiff.mod.f,
                                                                                        newdata = test, type = "response") %>% 
                                                                              mean(na.rm = T))


cor(test %>% 
      group_by(pitcher_name, pitch_name) %>% 
      summarise(whiff_plus = mean(whiff_plus, na.rm  = T),
                whiff_rate = round(sum(whiff)/sum(swing), 3),
                pitches = n()) %>% 
      filter(pitches > 50) %>% 
      drop_na(whiff_plus, whiff_rate) %>% 
      pull(whiff_plus),
    test %>% 
      group_by(pitcher_name, pitch_name) %>% 
      summarise(whiff_plus = mean(whiff_plus, na.rm  = T),
                whiff_rate = round(sum(whiff)/sum(swing), 3),
                pitches = n()) %>% 
      filter(pitches > 50) %>% 
      drop_na(whiff_plus, whiff_rate) %>% 
      pull(whiff_rate)); test %>% 
  group_by(pitcher_name, pitch_name) %>% 
  summarise(whiff_plus = mean(whiff_plus, na.rm  = T),
            whiff_rate = round(sum(whiff)/sum(swing), 3),
            pitches = n()) %>% 
  filter(pitches > 50) %>% 
  ggplot(aes(whiff_rate, whiff_plus)) +
  geom_point()



### offspeed stuff
whiff_data_off <- data_pitches %>% 
  mutate(desired_outcome = ifelse(description %in% c("called_strike", "swinging_strike", "foul_tip",
                                                     "swinging_strike_blocked", "missed_bunt"), 1, 0),
         pfx_x = pfx_x*-12,
         pfx_z = pfx_z*12,
         total_break = sqrt(pfx_x**2 + pfx_z**2)) %>% 
  filter(pitch_name != "" | pitch_name != "Pitch Out",
         pitch_group == "offspeed") %>% 
  left_join(velo_key, by = c("pitcher_name")) %>% 
  select(-c(pitch_name.y)) %>% 
  rename("pitch_name" = pitch_name.x) %>% 
  mutate(velo_diff = release_speed - avg_velo,
         h_break_diff = pfx_x - avg_h_break,
         v_break_diff = pfx_z - avg_v_break,
         vy_f = -sqrt(vy0**2 - (2*ay*(50-17/12))),
         t = (vy_f - vy0)/ay,
         vz_f = vz0 + (az*t),
         VAA = -atan(vz_f/vy_f)*(180/pi)) %>% 
  filter(swing == 1)


set.seed(21)
dt = sort(sample(nrow(whiff_data_off), nrow(whiff_data_off)*.7))
train<-whiff_data_off[dt,]
test<-whiff_data_off[-dt,]
test <- test %>%
  drop_na(release_speed, release_pos_x, release_pos_z, pfx_x, pfx_z)


whiff.mod.off <- glm(whiff ~  velo_diff:release_speed + h_break_diff:pfx_z + pfx_x:v_break_diff + VAA,
                     data = train %>% filter(pitch_group == "offspeed"), family = binomial())


summary(whiff.mod.off)


test$whiff_plus <- predict(whiff.mod.off, newdata = test, type = "response")*(100/predict(whiff.mod.off,
                                                                                          newdata = test, type = "response") %>% 
                                                                                mean(na.rm = T))


cor(test %>% 
      group_by(pitcher_name, pitch_name) %>% 
      summarise(whiff_plus = mean(whiff_plus, na.rm  = T),
                whiff_rate = round(sum(whiff)/sum(swing), 3),
                pitches = n()) %>% 
      drop_na(whiff_plus, whiff_rate) %>%
      pull(whiff_plus),
    test %>% 
      group_by(pitcher_name, pitch_name) %>% 
      summarise(whiff_plus = mean(whiff_plus, na.rm  = T),
                whiff_rate = round(sum(whiff)/sum(swing), 3),
                pitches = n()) %>% 
      drop_na(whiff_plus, whiff_rate) %>% 
      pull(whiff_rate)); test %>% 
  group_by(pitcher_name, pitch_name) %>% 
  summarise(whiff_plus = mean(whiff_plus, na.rm  = T),
            whiff_rate = round(sum(whiff)/sum(swing), 3),
            pitches = n()) %>% 
  filter(pitches > 20) %>% 
  ggplot(aes(whiff_rate, whiff_plus)) +
  geom_point()




### breaking stuff
whiff_data_brk <- data_pitches %>% 
  mutate(desired_outcome = ifelse(description %in% c("called_strike", "swinging_strike", "foul_tip",
                                                     "swinging_strike_blocked", "missed_bunt"), 1, 0),
         pfx_x = pfx_x*-12,
         pfx_z = pfx_z*12,
         total_break = sqrt(pfx_x**2 + pfx_z**2)) %>% 
  filter(pitch_name != "" | pitch_name != "Pitch Out",
         pitch_group == "breaking") %>% 
  left_join(velo_key, by = c("pitcher_name")) %>% 
  select(-c(pitch_name.y)) %>% 
  rename("pitch_name" = pitch_name.x) %>% 
  mutate(velo_diff = release_speed - avg_velo,
         h_break_diff = pfx_x - avg_h_break,
         v_break_diff = pfx_z - avg_v_break,
         vy_f = -sqrt(vy0**2 - (2*ay*(50-17/12))),
         t = (vy_f - vy0)/ay,
         vz_f = vz0 + (az*t),
         VAA = -atan(vz_f/vy_f)*(180/pi)) %>% 
  filter(swing == 1)


set.seed(21)
dt = sort(sample(nrow(whiff_data_brk), nrow(whiff_data_brk)*.7))
train<-whiff_data_brk[dt,]
test<-whiff_data_brk[-dt,]
test <- test %>%
  drop_na(release_speed, release_pos_x, release_pos_z, pfx_x, pfx_z)

whiff.mod.brk <- glm(whiff ~ h_break_diff + v_break_diff + release_speed*velo_diff +
                       spin_axis + pfx_x:pfx_z + vx0:ax + VAA,
                     data = train %>% filter(pitch_group == "breaking"), family = binomial())


summary(whiff.mod.brk)


test$whiff_plus <- predict(whiff.mod.brk, newdata = test, type = "response")*(100/predict(whiff.mod.brk,
                                                                                          newdata = test, type = "response") %>% 
                                                                                mean(na.rm = T))


cor(test %>% 
      group_by(pitcher_name, pitch_name) %>% 
      summarise(whiff_plus = mean(whiff_plus, na.rm  = T),
                whiff_rate = round(sum(whiff)/sum(swing), 3),
                pitches = n()) %>% 
      drop_na(whiff_plus, whiff_rate) %>%
      filter(pitches > 20) %>% 
      pull(whiff_plus),
    test %>% 
      group_by(pitcher_name, pitch_name) %>% 
      summarise(whiff_plus = mean(whiff_plus, na.rm  = T),
                whiff_rate = round(sum(whiff)/sum(swing), 3),
                pitches = n()) %>% 
      drop_na(whiff_plus, whiff_rate) %>% 
      filter(pitches > 20) %>% 
      pull(whiff_rate)); test %>% 
  group_by(pitcher_name, pitch_name) %>% 
  summarise(whiff_plus = mean(whiff_plus, na.rm  = T),
            whiff_rate = round(sum(whiff)/sum(swing), 3),
            pitches = n()) %>% 
  filter(pitches > 20) %>% 
  ggplot(aes(whiff_rate, whiff_plus)) +
  geom_point()




### create totals


dt = sort(sample(nrow(data_pitches), nrow(data_pitches)*.7))
train<-data_pitches[dt,]
test<-data_pitches[-dt,] %>% 
  mutate(pitch_group = case_when(
    pitch_name %in% c("Sweeper", "Curveball", "Slider", "Knucke Curve", "Slurve", "Eephus", "Slow Curve", "Knuckleball") ~ "breaking",
    pitch_name %in% c("4-Seam Fastball", "Sinker", "Cutter") ~ "fastball",
    pitch_name %in% c("Split-Finger", "Forkball", "Screwball", "Changeup") ~ "offspeed"
  )) %>% 
  left_join(velo_key, by = "pitcher_name") %>% 
  mutate(velo_diff = release_speed - avg_velo,
         h_break_diff = pfx_x - avg_h_break,
         v_break_diff = pfx_z - avg_v_break,
         total_break = sqrt(pfx_x**2 + pfx_z**2),
         vy_f = -sqrt(vy0**2 - (2*ay*(50-17/12))),
         t = (vy_f - vy0)/ay,
         vz_f = vz0 + (az*t),
         VAA = -atan(vz_f/vy_f)*(180/pi))


test[test$pitch_group == "fastball", "predicted_outcome"] <- predict(whiff.mod.f,
                                                                     newdata = test[test$pitch_group == "fastball", ],
                                                                     type = "response")*(100/predict(whiff.mod.f,
                                                                                                     newdata = test[test$pitch_group == "fastball", ],
                                                                                                     type = "response") %>% mean(na.rm = T))

test[test$pitch_group == "offspeed", "predicted_outcome"] <- predict(whiff.mod.off,
                                                                     newdata = test[test$pitch_group == "offspeed", ],
                                                                     type = "response")*(100/predict(whiff.mod.off,
                                                                                                     newdata = test[test$pitch_group == "offspeed", ],
                                                                                                     type = "response") %>% mean(na.rm = T))

test[test$pitch_group == "breaking", "predicted_outcome"] <- predict(whiff.mod.brk,
                                                                     newdata = test[test$pitch_group == "breaking", ],
                                                                     type = "response")*(100/predict(whiff.mod.brk,
                                                                                                     newdata = test[test$pitch_group == "breaking", ],
                                                                                                     type = "response") %>% mean(na.rm = T))


whiff_plus.dat <- test %>% 
  rename("pitch_name" = pitch_name.x) %>% 
  group_by(pitcher_name, pitch_name) %>% 
  summarise(whiff_plus = mean(predicted_outcome, na.rm = T))


### STUFF plus

ideal_outcomes <- c("strikeout", "field_out", "force_out", "grounded_into_double_play",
                    "fielders_choice")
  
### fastball stuff
stuff_data_f <- data_pitches %>% 
  mutate(pfx_x = pfx_x*-12,
         pfx_z = pfx_z*12,
         total_break = sqrt(pfx_x**2 + pfx_z**2),
         ideal_outcome = ifelse(whiff == 1 | events %in% ideal_outcomes, 1, 0)) %>% 
  filter(pitch_name != "" | pitch_name != "Pitch Out",
         pitch_group == "fastball") %>% 
  left_join(velo_off_key, by = c("pitcher_name")) %>% 
  select(-c(pitch_name.y)) %>% 
  rename("pitch_name" = pitch_name.x) %>% 
  mutate(velo_diff = release_speed - avg_velo,
         h_break_diff = pfx_x - avg_h_break,
         v_break_diff = pfx_z - avg_v_break,
         vy_f = -sqrt(vy0**2 - (2*ay*(50-17/12))),
         t = (vy_f - vy0)/ay,
         vz_f = vz0 + (az*t),
         VAA = -atan(vz_f/vy_f)*(180/pi)) %>% 
  filter(swing == 1 | actual_strike == 1)


set.seed(21)
dt = sort(sample(nrow(stuff_data_f), nrow(stuff_data_f)*.7))
train<-stuff_data_f[dt,]
test<-stuff_data_f[-dt,]
test <- test %>%
  drop_na(release_speed, release_pos_x, release_pos_z, pfx_x, pfx_z)

stuff.mod.f <- glm(ideal_outcome ~ spin_axis:release_spin_rate:release_speed + velo_diff + az*vz0 +
                     vy0*ay + vx0*ax + v_break_diff + pfx_x:pfx_z + VAA,
                   data = train %>% filter(pitch_group == "fastball"), family = binomial())


summary(stuff.mod.f)

test <- test %>% 
  drop_na(pfx_z, pitch_group)


test$stuff_plus <- predict(stuff.mod.f, newdata = test, type = "response")*(100/predict(stuff.mod.f,
                                                                                        newdata = test, type = "response") %>% 
                                                                              mean(na.rm = T))


cor(test %>% 
      group_by(pitcher_name, pitch_name) %>% 
      summarise(stuff_plus = mean(stuff_plus, na.rm  = T),
                whiff_rate = round(mean(ideal_outcome, na.rm = T), 3),
                pitches = n()) %>% 
      filter(pitches > 50) %>% 
      drop_na(stuff_plus, whiff_rate) %>% 
      pull(stuff_plus),
    test %>% 
      group_by(pitcher_name, pitch_name) %>% 
      summarise(stuff_plus = mean(stuff_plus, na.rm  = T),
                whiff_rate = round(mean(ideal_outcome, na.rm = T), 3),
                pitches = n()) %>% 
      filter(pitches > 50) %>% 
      drop_na(stuff_plus, whiff_rate) %>% 
      pull(whiff_rate)); test %>% 
  group_by(pitcher_name, pitch_name) %>% 
  summarise(stuff_plus = mean(stuff_plus, na.rm  = T),
            whiff_rate = round(mean(ideal_outcome, na.rm = T), 3),
            pitches = n()) %>% 
  filter(pitches > 50) %>% 
  ggplot(aes(whiff_rate, stuff_plus)) +
  geom_point()



### offspeed stuff
stuff_data_off <- data_pitches %>% 
  mutate(pfx_x = pfx_x*-12,
         pfx_z = pfx_z*12,
         total_break = sqrt(pfx_x**2 + pfx_z**2),
         ideal_outcome = ifelse(whiff == 1 | events %in% ideal_outcomes, 1, 0)) %>% 
  filter(pitch_name != "" | pitch_name != "Pitch Out",
         pitch_group == "offspeed") %>% 
  left_join(velo_key, by = c("pitcher_name")) %>% 
  select(-c(pitch_name.y)) %>% 
  rename("pitch_name" = pitch_name.x) %>% 
  mutate(velo_diff = release_speed - avg_velo,
         h_break_diff = pfx_x - avg_h_break,
         v_break_diff = pfx_z - avg_v_break,
         vy_f = -sqrt(vy0**2 - (2*ay*(50-17/12))),
         t = (vy_f - vy0)/ay,
         vz_f = vz0 + (az*t),
         VAA = -atan(vz_f/vy_f)*(180/pi)) %>% 
  filter(swing == 1 | actual_strike == 1)


set.seed(21)
dt = sort(sample(nrow(stuff_data_off), nrow(stuff_data_off)*.7))
train<-stuff_data_off[dt,]
test<-stuff_data_off[-dt,]
test <- test %>%
  drop_na(release_speed, release_pos_x, release_pos_z, pfx_x, pfx_z)


stuff.mod.off <- glm(ideal_outcome ~  spin_axis*release_spin_rate*release_speed + velo_diff + az*vz0 +
                       vy0*ay + vx0*ax + v_break_diff:h_break_diff + VAA,
                     data = train %>% filter(pitch_group == "offspeed"), family = binomial())


summary(stuff.mod.off)


test$stuff_plus <- predict(stuff.mod.off, newdata = test, type = "response")*(100/predict(stuff.mod.off,
                                                                                          newdata = test, type = "response") %>% 
                                                                                mean(na.rm = T))


cor(test %>% 
      group_by(pitcher_name, pitch_name) %>% 
      summarise(stuff_plus = mean(stuff_plus, na.rm  = T),
                whiff_rate = round(mean(ideal_outcome, na.rm = T), 3),
                pitches = n()) %>% 
      drop_na(stuff_plus, whiff_rate) %>%
      pull(stuff_plus),
    test %>% 
      group_by(pitcher_name, pitch_name) %>% 
      summarise(stuff_plus = mean(stuff_plus, na.rm  = T),
                whiff_rate = round(mean(ideal_outcome, na.rm = T), 3),
                pitches = n()) %>% 
      drop_na(stuff_plus, whiff_rate) %>% 
      pull(whiff_rate)); test %>% 
  group_by(pitcher_name, pitch_name) %>% 
  summarise(stuff_plus = mean(stuff_plus, na.rm  = T),
            whiff_rate = round(mean(ideal_outcome, na.rm = T), 3),
            pitches = n()) %>% 
  filter(pitches > 20) %>% 
  ggplot(aes(whiff_rate, stuff_plus)) +
  geom_point()

test %>% 
  group_by(pitcher_name, pitch_name) %>% 
  summarise(stuff_plus = mean(stuff_plus, na.rm  = T),
            whiff_rate = round(mean(ideal_outcome, na.rm = T), 3),
            pitches = n()) %>% view()


### breaking stuff
stuff_data_brk <- data_pitches %>% 
  mutate(ideal_outcome = ifelse(whiff == 1 | events %in% ideal_outcomes, 1, 0),
         pfx_x = pfx_x*-12,
         pfx_z = pfx_z*12,
         total_break = sqrt(pfx_x**2 + pfx_z**2)) %>% 
  filter(pitch_name != "" | pitch_name != "Pitch Out",
         pitch_group == "breaking") %>% 
  left_join(velo_key, by = c("pitcher_name")) %>% 
  select(-c(pitch_name.y)) %>% 
  rename("pitch_name" = pitch_name.x) %>% 
  mutate(velo_diff = release_speed - avg_velo,
         h_break_diff = pfx_x - avg_h_break,
         v_break_diff = pfx_z - avg_v_break,
         vy_f = -sqrt(vy0**2 - (2*ay*(50-17/12))),
         t = (vy_f - vy0)/ay,
         vz_f = vz0 + (az*t),
         VAA = -atan(vz_f/vy_f)*(180/pi)) %>% 
  filter(swing == 1 | actual_strike == 1)


set.seed(21)
dt = sort(sample(nrow(stuff_data_brk), nrow(stuff_data_brk)*.7))
train<-stuff_data_brk[dt,]
test<-stuff_data_brk[-dt,]
test <- test %>%
  drop_na(release_speed, release_pos_x, release_pos_z, pfx_x, pfx_z)

stuff.mod.brk <- glm(ideal_outcome ~ velo_diff + az*vz0 + VAA +
                       vy0*ay + vx0*ax + v_break_diff:h_break_diff + release_pos_z:release_pos_y,
                     data = train %>% filter(pitch_group == "breaking"), family = binomial())


summary(stuff.mod.brk)


test$stuff_plus <- predict(stuff.mod.brk, newdata = test, type = "response")*(100/predict(stuff.mod.brk,
                                                                                          newdata = test, type = "response") %>% 
                                                                                mean(na.rm = T))


cor(test %>% 
      group_by(pitcher_name, pitch_name) %>% 
      summarise(stuff_plus = mean(stuff_plus, na.rm  = T),
                whiff_rate = round(mean(ideal_outcome, na.rm = T), 3),
                pitches = n()) %>% 
      filter(pitches > 20) %>% 
      drop_na(stuff_plus, whiff_rate) %>%
      pull(stuff_plus),
    test %>% 
      group_by(pitcher_name, pitch_name) %>% 
      summarise(stuff_plus = mean(stuff_plus, na.rm  = T),
                whiff_rate = round(mean(ideal_outcome, na.rm = T), 3),
                pitches = n()) %>% 
      filter(pitches > 20) %>% 
      drop_na(stuff_plus, whiff_rate) %>% 
      pull(whiff_rate)); test %>% 
  group_by(pitcher_name, pitch_name) %>% 
  summarise(stuff_plus = mean(stuff_plus, na.rm  = T),
            whiff_rate = round(mean(ideal_outcome, na.rm = T), 3),
            pitches = n()) %>% 
  filter(pitches > 20) %>% 
  ggplot(aes(whiff_rate, stuff_plus)) +
  geom_point() + geom_smooth(method = "lm")



### create totals


dt = sort(sample(nrow(data_pitches), nrow(data_pitches)*.7))
train<-data_pitches[dt,]
test<-data_pitches[-dt,] %>% 
  mutate(pitch_group = case_when(
    pitch_name %in% c("Sweeper", "Curveball", "Slider", "Knucke Curve", "Slurve", "Eephus", "Slow Curve", "Knuckleball") ~ "breaking",
    pitch_name %in% c("4-Seam Fastball", "Sinker", "Cutter") ~ "fastball",
    pitch_name %in% c("Split-Finger", "Forkball", "Screwball", "Changeup") ~ "offspeed"
  )) %>% 
  left_join(velo_key, by = "pitcher_name") %>% 
  mutate(velo_diff = release_speed - avg_velo,
         h_break_diff = pfx_x - avg_h_break,
         v_break_diff = pfx_z - avg_v_break,
         vy_f = -sqrt(vy0**2 - (2*ay*(50-17/12))),
         t = (vy_f - vy0)/ay,
         vz_f = vz0 + (az*t),
         VAA = -atan(vz_f/vy_f)*(180/pi),
         total_break = sqrt(pfx_x**2 + pfx_z**2))


test[test$pitch_group == "fastball", "predicted_outcome"] <- predict(stuff.mod.f,
                                                                     newdata = test[test$pitch_group == "fastball", ],
                                                                     type = "response")*(100/predict(stuff.mod.f,
                                                                                                     newdata = test[test$pitch_group == "fastball", ],
                                                                                                     type = "response") %>% mean(na.rm = T))

test[test$pitch_group == "offspeed", "predicted_outcome"] <- predict(stuff.mod.off,
                                                                     newdata = test[test$pitch_group == "offspeed", ],
                                                                     type = "response")*(100/predict(stuff.mod.off,
                                                                                                     newdata = test[test$pitch_group == "offspeed", ],
                                                                                                     type = "response") %>% mean(na.rm = T))

test[test$pitch_group == "breaking", "predicted_outcome"] <- predict(stuff.mod.brk,
                                                                     newdata = test[test$pitch_group == "breaking", ],
                                                                     type = "response")*(100/predict(stuff.mod.brk,
                                                                                                     newdata = test[test$pitch_group == "breaking", ],
                                                                                                     type = "response") %>% mean(na.rm = T))

stuff_plus.dat <- test %>% 
  rename("pitch_name" = pitch_name.x) %>% 
  group_by(pitcher_name, pitch_name) %>% 
  summarise(stuff_plus = mean(predicted_outcome, na.rm = T))


### add into function


filter_statcast_pitcher_pitches <- function(date = "2023-03-30", pa_limit, starters_only = F){
  data_pitches.filter <- data_pitches %>% 
    filter(game_date >= date)
  
  data_results.filter <- data_pitches.filter %>% 
    filter(events != "")
  
  result_stats <- data_results.filter %>% 
    group_by(pitcher_name, pitch_name) %>% 
    summarise(results = n(),
              barrels = sum(barrel, na.rm = TRUE),
              barrel_rate = sum(barrel, na.rm = TRUE) / sum(!is.na(barrel)),
              hard_hit_rate = sum(hard_hit, na.rm = TRUE) / sum(!is.na(hard_hit)),
              wOBA = (0.690 * sum(events == "walk") +
                        0.72 * sum(events == "hit_by_pitch") +
                        .89* sum(events == "single") +
                        1.27 * sum(events == "double") +
                        1.62 * sum(events == "triple") +
                        2.1 * sum(events == "home_run")) / (results),
              xwOBA = mean(xwOBA, na.rm = T),
              LD_percent = sum(bb_type == "line_drive")/n(),
              GB_percent = sum(bb_type == "ground_ball")/n(),
              FB_percent = sum(bb_type == "fly_ball" | bb_type == "popup")/n()
    )
  
  days_used <- as.numeric(Sys.Date() - as.Date(date))
  
  if(starters_only == T){data_pitches.filter %>% 
      group_by(pitcher_name, pitch_name) %>% 
      summarise(pitches = n(),
                avg_velo = mean(release_speed, na.rm = T),
                whiff_rate = round(sum(whiff)/sum(swing), 3),
                chase_rate = round(sum(chase, na.rm = T)/sum(actual_strike == 0, na.rm = T),3),
                horizontal_break = mean(pfx_x*-12, na.rm = T),
                vertical_break = mean(pfx_z*12, na.rm = T),
                starter = ifelse(min(inning) == 1, 1, 0),
                strike_rate = sum(actual_strike == 1)/pitches) %>% 
      left_join(whiff_plus.dat, by = c("pitcher_name", "pitch_name")) %>% 
      left_join(stuff_plus.dat, by = c("pitcher_name", "pitch_name")) %>% 
      left_join(result_stats) %>% 
      drop_na(pitches) %>% 
      relocate(pitcher_name, pitch_name, pitches, whiff_plus, stuff_plus) %>% 
      filter(starter == 1) %>% select(-c(starter, results)) %>% 
      return()}
  
  else{
    data_pitches.filter %>% 
      group_by(pitcher_name, pitch_name) %>% 
      summarise(pitches = n(),
                avg_velo = mean(release_speed, na.rm = T),
                whiff_rate = round(sum(whiff)/sum(swing), 3),
                chase_rate = round(sum(chase, na.rm = T)/sum(actual_strike == 0, na.rm = T),3),
                horizontal_break = mean(pfx_x*-12, na.rm = T),
                vertical_break = mean(pfx_z*12, na.rm = T),
                strike_rate = sum(actual_strike == 1)/pitches) %>% 
      left_join(whiff_plus.dat, by = c("pitcher_name", "pitch_name")) %>% 
      left_join(stuff_plus.dat, by = c("pitcher_name", "pitch_name")) %>% 
      left_join(result_stats) %>% 
      drop_na(pitches) %>% 
      relocate(pitcher_name, pitch_name, pitches, whiff_plus, stuff_plus) %>% 
      select(-results) %>% 
      return()
  }
}


round3 <- function(x){
  return(round(x, 3))
}

filter_statcast_pitcher_pitches(starters_only = T) %>% mutate_if(is.numeric, round3) %>% 
write_rds("predictors/pitch_level_start.rds")


filter_statcast_pitcher_pitches(starters_only = F) %>% mutate_if(is.numeric, round3) %>%
  write_rds("predictors/pitch_level_all.rds")


filter_statcast_team_hitting(Sys.Date()-30) %>% 
  write_rds("predictors/hit_team.rds")


filter_statcast_team_pitching(Sys.Date()-30) %>% 
  write_rds("predictors/pitch_team.rds")




write_rds(data_pitches, "predictors/data_pitches.rds")





data_pitches %>% 
  group_by(pitcher_name, pitch_name) %>% 
  summarise(avg_strike_pct = sum(actual_strike == 1 | whiff == 1 | chase == 1)/n()) %>% 
  left_join(data_pitches %>% 
              group_by(pitch_name) %>% 
              summarise(strike_rate = sum(actual_strike == 1 | whiff == 1 | chase == 1)/n()),
            by = "pitch_name") %>% 
  mutate(strike_rate_plus = avg_strike_pct/strike_rate*100) %>% view()


  
loc_data_f <- data_pitches %>% 
  mutate(pfx_x = pfx_x*-12,
         pfx_z = pfx_z*12,
         total_break = sqrt(pfx_x**2 + pfx_z**2),
         ideal_outcome = ifelse(whiff == 1 | events %in% ideal_outcomes, 1, 0)) %>% 
  filter(pitch_name != "" | pitch_name != "Pitch Out",
         pitch_group == "fastball") %>% 
  left_join(velo_off_key, by = c("pitcher_name")) %>% 
  select(-c(pitch_name.y)) %>% 
  rename("pitch_name" = pitch_name.x) %>% 
  mutate(velo_diff = release_speed - avg_velo,
         h_break_diff = pfx_x - avg_h_break,
         v_break_diff = pfx_z - avg_v_break)


set.seed(21)
dt = sort(sample(nrow(stuff_data_f), nrow(stuff_data_f)*.7))
train<-stuff_data_f[dt,]
test<-stuff_data_f[-dt,]
test <- test %>%
  drop_na(release_speed, release_pos_x, release_pos_z, pfx_x, pfx_z)

loc.mod.f <- glmer(ideal_outcome ~ spin_axis:release_spin_rate:release_speed + velo_diff + az*vz0 +
                     vy0*ay + vx0*ax + v_break_diff + pfx_x:pfx_z + (1|zone),
                   data = train %>% filter(pitch_group == "fastball"), family = binomial())
  
  
