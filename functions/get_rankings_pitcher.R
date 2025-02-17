hits <- c("double", "single", "triple", "home_run")
pa_ab <- c("catcher_interf", "caught_stealing_2b", "caught_stealing_3b",
           "caught_stealing_home", "hit_by_pitch", "pickoff_1b", "pickoff_3b", 
           "pickoff_caught_stealing_2b", "stolen_base_2b", "sac_bunt", "sac_fly",
           "sac_fly_double_play", "wild_pitch", "walk")

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

get_rankings_pitcher("Gerrit Cole", date = Sys.Date() - 30)
