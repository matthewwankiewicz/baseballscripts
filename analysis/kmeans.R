## starter cluster analysis

# First, identify starting pitchers
starter_data <- data2024 %>%
  group_by(pitcher_name, game_pk) %>%
  summarize(pitches_per_game = n()) %>%
  filter(pitches_per_game >= 50) %>%  # Rough filter for starters
  select(pitcher_name) %>%
  distinct()

# Create pitcher profiles
pitcher_profiles <- data2024 %>%
  inner_join(starter_data, by = "pitcher_name") %>%
  group_by(pitcher_name) %>%
  summarise(
    # Velocity & Movement
    avg_fastball_velo = mean(release_speed[pitch_group=='fastball'], na.rm = TRUE),
    avg_spin_rate = mean(release_spin_rate, na.rm = TRUE),
    
    # Command metrics
    zone_rate = mean(zone >= 1 & zone <= 9, na.rm = TRUE),
    edge_rate = mean(zone %in% c(11:14), na.rm = TRUE),
    
    # Movement metrics
    avg_vertical_break = mean(pfx_z, na.rm = TRUE),
    avg_horizontal_break = mean(pfx_x, na.rm = TRUE),
    
    # Outcome metrics
    whiff_rate = mean(whiff[swing == TRUE], na.rm = TRUE),
    avg_launch_speed = mean(launch_speed, na.rm = TRUE),
    
    # Extension
    avg_extension = mean(release_extension, na.rm = TRUE)
  )


### kmeans
# First, let's standardize the numerical columns
pitcher_scaled <- pitcher_profiles %>%
  select(-pitcher_name) %>% # Remove non-numeric column
  scale()

# Let's determine optimal number of clusters
library(factoextra)

# Elbow method to find optimal k
set.seed(42)
fviz_nbclust(pitcher_scaled, kmeans, method = "wss", k.max = 10)

# Perform k-means clustering (let's start with k=3)
set.seed(42)
kmeans_result <- kmeans(pitcher_scaled, centers = 3, nstart = 25)

# Add cluster assignments back to original data
pitcher_profiles$cluster <- kmeans_result$cluster

# Look at cluster characteristics
cluster_summary <- pitcher_profiles %>%
  group_by(cluster) %>%
  summarise(
    n_pitchers = n(),
    avg_fastball_velo = mean(avg_fastball_velo),
    avg_spin_rate = mean(avg_spin_rate),
    zone_rate = mean(zone_rate),
    whiff_rate = mean(whiff_rate),
    avg_extension = mean(avg_extension)
  )



cluster_summary %>% data.table(
  
)


pitcher_profiles %>% View



