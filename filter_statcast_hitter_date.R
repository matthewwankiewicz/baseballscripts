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