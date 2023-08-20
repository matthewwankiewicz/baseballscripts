hits <- c("double", "single", "triple", "home_run")
pa_ab <- c("catcher_interf", "caught_stealing_2b", "caught_stealing_3b",
           "caught_stealing_home", "hit_by_pitch", "pickoff_1b", "pickoff_3b", 
           "pickoff_caught_stealing_2b", "stolen_base_2b", "sac_bunt", "sac_fly",
           "sac_fly_double_play", "wild_pitch", "walk")

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