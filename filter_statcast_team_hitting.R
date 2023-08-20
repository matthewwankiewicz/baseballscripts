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