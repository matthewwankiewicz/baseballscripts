## starter cluster analysis

# First, identify starting pitchers
starter_data <- data2024 %>%
  group_by(pitcher_name, game_pk) %>%
  summarize(pitches_per_game = mean(n())) %>%
  group_by(pitcher_name) %>% 
  summarise(avg_pitch = mean(pitches_per_game)) %>% 
  filter(avg_pitch>=50) %>% 
  distinct(pitcher_name)

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
kmeans_result <- kmeans(pitcher_scaled, centers = 4, nstart = 25)

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




pitcher_profiles %>% View



#### cluster hitters ######
# pull batter names
batter_names <- data2024 %>% 
  filter(events!="") %>% 
  group_by(batter_name) %>% 
  filter(n()>200) %>% pull(batter_name) %>% unique()


## filter data for batters, get:
### whiff, chase, barrel, pulled fly ball, 
batter_stats <- data2024 %>% 
  filter(game_date >= "2024-03-28") %>% 
  filter(batter_name %in% batter_names) %>% 
  group_by(batter_name) %>% 
  summarise(whiff_rate = mean(whiff[swing==TRUE], na.rm=TRUE),
            chase_rate = mean(chase[actual_strike==FALSE], na.rm=TRUE),
            swing_rate = mean(swing, na.rm=TRUE),
            barrel_rate = mean(barrel[swing==TRUE], na.rm=TRUE),
            gb_rate = mean(bb_type[bb_type!='']=='ground_ball', na.rm=TRUE),
            bb_rate = mean(events[events!=''] == 'walk', na.rm=TRUE),
            k_rate = mean(events[events!=''] == 'strikeout', na.rm=TRUE),
            bb_k_rate = bb_rate/k_rate,
            hard_hit_rate = mean(hard_hit[bb_type!=''], na.rm=TRUE),
            pull_pct = mean(hit_type[bb_type!='']=='pull', na.rm=TRUE),
            fb_rate = mean(bb_type[bb_type!='']=='fly_ball', na.rm=TRUE),
            singles = sum(events == "single", na.rm = TRUE),
            doubles = sum(events == "double", na.rm = TRUE),
            triples = sum(events == "triple", na.rm = TRUE),
            home_runs = sum(events == "home_run", na.rm = TRUE),
            at_bats = sum(type=='X'|events=='strikeout'),
            hits = singles + doubles + triples + home_runs) %>% 
  mutate(xbh = doubles + triples + home_runs,
         xbh_pct = xbh / hits,
         TB = singles + (2 * doubles) + (3 * triples) + (4 * home_runs),
         BA = hits / at_bats,
         ISO = (TB / at_bats) - BA
  ) %>% 
  select(batter_name, whiff_rate, ISO, bb_k_rate, barrel_rate, hard_hit_rate,
         chase_rate)




## select num columns, scale
df_scaled <- batter_stats %>% 
  select(-batter_name) %>% 
  scale()


## # Determine optimal clusters using the elbow method
wss <- map_dbl(1:10, ~kmeans(df_scaled, .x, nstart = 25)$tot.withinss)

# Plot elbow method
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters", ylab = "Total Within-Cluster Sum of Squares")

# Perform K-means clustering with chosen k (e.g., k = 3)
set.seed(21)  # Ensure reproducibility
kmeans_result <- kmeans(df_scaled, centers = 4, nstart = 25)

## assign clusters
batter_stats$cluster <- kmeans_result$cluster


## look at averages by cluster
batter_stats %>% 
  group_by(cluster) %>% 
  summarise(across(everything(), mean, na.rm=TRUE)) %>% data.table::data.table() %>% 
  select(-batter_name) %>% 
  summarise(across(everything(), rank))


### apply to minor league data
milb_batter_names <- minor_leagues %>% 
  filter(last.pitch.of.ab=='true' & level=='aaa') %>% 
  group_by(batter_name) %>% 
  filter(n()>200) %>% pull(batter_name) %>% unique()


milb_batter_stats <- minor_leagues %>% 
  filter(level=='aaa') %>% 
  group_by(batter_name) %>% 
  filter(n() >= 100) %>% 
  summarise(whiff_rate = mean(whiff[swing==TRUE], na.rm=TRUE),
            chase_rate = mean(chase[actual_strike==FALSE], na.rm=TRUE),
            swing_rate = mean(swing, na.rm=TRUE),
            barrel_rate = mean(barrel, na.rm=TRUE),
            pull_pct = mean(hit_type[bb_type!='']=='pull', na.rm=TRUE),
            hard_hit_rate = mean(hard_hit[bb_type!=''], na.rm = TRUE),
            gb_rate = mean(bb_type[bb_type!='']=='ground_ball', na.rm=TRUE),
            bb_rate = mean(events == 'Walk', na.rm=TRUE),
            k_rate = mean(events == 'Strikeout', na.rm=TRUE),
            bb_k_rate = bb_rate/k_rate,
            hard_hit_rate = mean(hard_hit[bb_type!=''], na.rm=TRUE),
            pull_pct = mean(hit_type[bb_type!='']=='pull', na.rm=TRUE),
            fb_rate = mean(bb_type[bb_type!='']=='fly_ball', na.rm=TRUE),
            singles = sum(events == "Single", na.rm = TRUE),
            doubles = sum(events == "Double", na.rm = TRUE),
            triples = sum(events == "Triple", na.rm = TRUE),
            home_runs = sum(events == "Home Run", na.rm = TRUE),
            at_bats = sum(!is.na(bb_type)|events=='Strikeout'),
            hits = singles + doubles + triples + home_runs) %>% 
  mutate(xbh = doubles + triples + home_runs,
         xbh_pct = xbh / hits,
         TB = singles + (2 * doubles) + (3 * triples) + (4 * home_runs),
         BA = hits / at_bats,
         ISO = (TB / at_bats) - BA
  ) %>% 
  select(batter_name, whiff_rate, ISO, bb_k_rate, barrel_rate, hard_hit_rate,
         chase_rate)


## select num columns
df_scaled_minor <- milb_batter_stats %>% 
  select(-batter_name) %>% 
  scale()

## assign clusters
milb_clusters <- apply(df_scaled_minor, 1, function(x) {
  which.min(colSums((t(kmeans_result$centers) - x)^2))
})


milb_batter_stats$cluster <- milb_clusters

jays <- minor_leagues %>% 
  filter(batting_team=='Buffalo Bisons') %>% 
  pull(batter_name) %>% unique()


milb_batter_stats %>% 
  filter(batter_name%in%jays) %>% View
