### ip to outs ####
ip_to_outs <- function(ip){
  
  ip_split <- strsplit(as.character(ip),"\\.")
  
  if(length(ip_split[[1]]) == 1){
    as.numeric(ip_split[[1]][1])*3
  }
  
  else{
    as.numeric(ip_split[[1]][1])*3 + as.numeric(ip_split[[1]][2])
  }
  
  
}


### create pitcher plot ####
render_pitcher_plot <- function(pitch_name, season = year(Sys.Date())){
  
  id <- key_ids %>% 
    filter(name == pitch_name) %>% drop_na(key_fangraphs) %>% 
    pull(key_fangraphs)

  if(length(id) == 0){
    name_split <- str_split(pitch_name, " ")
    link <- paste0("https://www.fangraphs.com/search?q=", name_split[[1]][1], 
                   "%20", name_split[[1]][2])
    id <- readline(prompt = paste("Go to link and find id", link))
    key_ids[name == pitch_name, key_fangraphs := id]
  }

  data <- pitcher_game_logs_fg(playerid = id, year = season) %>% 
    mutate(OUTS = ip_to_outs(IP),
           QS = ifelse(IP >= 6 & ER <= 3, 1, 0),
           PTS = 4*W - 4*L + 8*CG + 8*ShO + 0.5*OUTS - 1.5*H - 3*ER -
             1.5*BB - 1.5*HBP + 2.5*SO + 6*QS + 6*SV + 4*HLD,
           month = format(as.Date(Date, format="%Y-%m-%d"),"%m"),
           five_game = (zoo::rollmean(PTS, k = 5, align = "left", fill = NA)),
           ten_game = (zoo::rollmean(PTS, k = 1, align = "left", fill = NA)),
           three_game = (zoo::rollmean(PTS, k = 3, align = "left", fill = NA)),
           Date = as.Date(Date))
  
  
  data %>% 
    ggplot() +
    geom_hline(yintercept = mean(data$PTS)) +
    geom_line(aes(x = Date, y = ten_game, color = "OneGmAvg")) +
    geom_line(aes(x = Date, y = five_game, color = "FiveGmAvg")) +
    geom_line(aes(x = Date, y = three_game, color = "ThreeGmAvg")) +
    ggtitle(label = "Rolling Avgs") +
    labs(color = "Legend") +
    theme_minimal() %>% 
    return()
}
