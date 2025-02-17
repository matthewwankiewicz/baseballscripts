hits <- c("double", "single", "triple", "home_run")
pa_ab <- c("catcher_interf", "caught_stealing_2b", "caught_stealing_3b",
           "caught_stealing_home", "hit_by_pitch", "pickoff_1b", "pickoff_3b", 
           "pickoff_caught_stealing_2b", "stolen_base_2b", "sac_bunt", "sac_fly",
           "sac_fly_double_play", "wild_pitch", "walk")

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
