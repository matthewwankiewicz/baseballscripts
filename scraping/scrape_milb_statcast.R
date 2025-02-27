## pull minor league statcast data
library(baseballr)
library(tidyverse)

# Generate a sequence of all days in 2024
dates_2024 <- as.character(seq(as.Date("2024-04-05"), as.Date("2024-09-30"), by = "day")) # MiLB season range

# Define minor league levels (Triple-A, Double-A, High-A, Single-A)
milb_levels <- c(12) 

# Initialize a list to store game_pks
all_games <- list()

for (date in dates_2024) {
  try({
    print(paste("Scraping data for:", date))
    games <- get_game_pks_mlb(date = date, level_ids = milb_levels)
    if (!is.null(games)) {
      all_games[[as.character(date)]] <- games
    }
  }, silent = TRUE)
}

# Combine all game data into a single dataframe
game_pks_df <- bind_rows(all_games)


#### scrape pitch by pitch
pbp_list <- list()

# Loop through each game_pk
for (pk in game_pks_df$game_pk) {
  print(game_pks_df %>% filter(game_pk == pk) %>% pull(officialDate))
  try({
    pbp_list[[as.character(pk)]] <- mlb_pbp(game_pk = pk)
  }, silent = TRUE)
}


# Combine all pitch data into a single dataframe
all_pbp_data <- bind_rows(pbp_list)


## save file
saveRDS(all_pbp_data, "aa_pitch_data.rds")
