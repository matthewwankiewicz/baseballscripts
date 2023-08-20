### get moving averages (sortable)
get_moving_averages <- function(stat){
  statcast_gamelogs %>% 
    mutate(
      PTS = 1.5 * singles + 3 * doubles + 4.5 * triples + 6 * home_runs + 1.5 * walks - 1.5 * strikeouts,
      tb = singles + 2 * doubles + 3 * triples + 4 * home_runs,
      five_game = (zoo::rollmean(!!sym(stat)*(1-k_rate)*(1+bb_rate), k = 5, align = "right", fill = NA)),
      ten_game = (zoo::rollmean(!!sym(stat)*(1-k_rate)*(1+bb_rate), k = 10, align = "right", fill = NA)),
      three_game = (zoo::rollmean(!!sym(stat)*(1-k_rate)*(1+bb_rate), k = 3, align = "right", fill = NA)),
      fifteen_game = (zoo::rollmean(!!sym(stat)*(1-k_rate)*(1+bb_rate), k = 15, align = "right", fill = NA))
    ) %>% 
    arrange(desc(game_date)) %>% 
    relocate(fifteen_game) %>% 
    distinct(batter_name, .keep_all = TRUE)
}


### moving average plotting

get_moving_averages2 <- function(stat) {
  statcast_gamelogs %>%
    mutate(
      PTS = 1.5 * singles + 3 * doubles + 4.5 * triples + 6 * home_runs + 1.5 * walks - 1.5 * strikeouts,
      tb = singles + 2 * doubles + 3 * triples + 4 * home_runs,
      five_game = zoo::rollmean(!!sym(stat) * (1 - k_rate) * (1 + bb_rate), k = 5, align = "right", fill = NA),
      ten_game = zoo::rollmean(!!sym(stat) * (1 - k_rate) * (1 + bb_rate), k = 10, align = "right", fill = NA),
      three_game = zoo::rollmean(!!sym(stat) * (1 - k_rate) * (1 + bb_rate), k = 3, align = "right", fill = NA),
      fifteen_game = zoo::rollmean(!!sym(stat) * (1 - k_rate) * (1 + bb_rate), k = 15, align = "right", fill = NA)
    ) %>%
    arrange(desc(game_date)) %>%
    relocate(fifteen_game)
}