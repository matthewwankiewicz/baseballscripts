### PROBABLES

game_pks <- mlb_game_pks(Sys.Date()) %>% pull(game_pk)


starters <- c()
for(game in game_pks){
  starters_name <- get_probables_mlb(game) %>% pull(fullName)
  starters <- c(starters, starters_name)
}



tibble(starters) %>% 
  left_join(stuff_plus.dat %>% 
              mutate(pitcher_name = stringi::stri_trans_general(str = pitcher_name, 
                                                                id = "Latin-ASCII")), by = c("starters" = "pitcher_name")) %>% 
  filter(pitch_name != "",
         !is.na(stuff_plus)) %>%
  left_join(whiff_plus.dat %>% 
              mutate(pitcher_name = stringi::stri_trans_general(str = pitcher_name, 
                                                                id = "Latin-ASCII")), by = c("starters" = "pitcher_name", "pitch_name")) %>% 
  write_rds("predictors/probables.rds")
