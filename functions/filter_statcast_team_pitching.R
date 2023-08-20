hits <- c("double", "single", "triple", "home_run")
pa_ab <- c("catcher_interf", "caught_stealing_2b", "caught_stealing_3b",
           "caught_stealing_home", "hit_by_pitch", "pickoff_1b", "pickoff_3b", 
           "pickoff_caught_stealing_2b", "stolen_base_2b", "sac_bunt", "sac_fly",
           "sac_fly_double_play", "wild_pitch", "walk")

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