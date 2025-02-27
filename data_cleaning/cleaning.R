library(baseballr)
library(tidyverse)
library(lubridate)

# Function to safely pull Statcast data
get_statcast_data_safe <- function(start_date, end_date) {
  if (is.na(end_date)) {
    end_date <- as.Date("2024-09-30")  # Default to last date
  }
  
  Sys.sleep(3)  # Prevent rate limiting
  tryCatch(
    {
      message("Pulling data from ", start_date, " to ", end_date)
      data <- statcast_search(start_date = start_date, end_date = end_date)
      
      if (nrow(data) == 0) {
        warning("No data found for ", start_date, " to ", end_date)
        return(NULL)
      }
      
      return(data)
    },
    error = function(e) {
      message("Error on ", start_date, " to ", end_date, ": ", e$message)
      return(NULL)  # Return NULL for failed requests
    }
  )
}

# Generate date ranges (3-day intervals)
date_ranges <- seq(as.Date("2024-03-20"), as.Date("2024-09-30"), by = "3 days")

# Initialize list to store batches
data_list <- vector("list", length(date_ranges) - 1)

# Loop through date ranges and store each batch in a list
for (i in seq_along(date_ranges)[-length(date_ranges)]) {
  start_date <- date_ranges[i]
  end_date <- if (i == length(date_ranges) - 1) as.Date("2024-09-30") else date_ranges[i + 1] - 1
  
  # Save each batch in the list
  data_list[[i]] <- get_statcast_data_safe(start_date, end_date)
  
  # Save progress every 10 batches
  if (i %% 10 == 0) {
    temp_data <- bind_rows(data_list)  # Combine current results
    saveRDS(temp_data, file = paste0("statcast_partial_", i, ".rds"))
    message("Saved partial results at batch ", i)
  }
}

# Combine all results safely
data2024 <- bind_rows(data_list) %>%
  drop_na(game_date)  # Remove failed requests

# Save final dataset
saveRDS(data2024, file = "statcast_2024.rds")


## clean data
data2024 <- data2024[!duplicated(data2024), ]


## get player id table
useable_data <- readRDS('/Users/matthew/Library/Mobile Documents/com~apple~CloudDocs/Documents/statcast_2024.rds')

player_table <- chadwick_player_lu()

statcast_ids <- player_table %>% 
  mutate(name = paste(name_first, name_last, sep = " ")) %>% 
  select(name, key_mlbam)

useable_data %>% 
  left_join(statcast_ids, by = c("pitcher" = "key_mlbam")) %>% 
  relocate(name) %>% 
  rename("pitcher_name" = name) %>%
  select(-player_name) %>% 
  left_join(statcast_ids, by = c("batter" = "key_mlbam")) %>% 
  rename("batter_name" = name) %>% 
  relocate(batter_name) -> usable_data


## edit copied data here
usable_data %>% code_barrel() %>% 
  mutate(hard_hit = ifelse(launch_speed >= 95, 1, 0),
         sz_left = -0.83,
         sz_right = 0.83,
         actual_strike = ifelse(zone <= 9, 1, 0),
         whiff = ifelse(description == "swinging_strike" | description == "swinging_strike_blocked" | description == "foul_tip", 1, 0),
         swing = ifelse(description != "pitchout" & description != "ball" & description != "called_strike" &  description != "blocked_ball" & description != "hit_by_pitch", 1, 0),
         chase = ifelse(swing == 1 & actual_strike == 0,1,0),
         zone_swing = ifelse(swing == 1 & actual_strike == 1, 1, 0),
         hitting_team = ifelse(inning_topbot == "Top", away_team, home_team),
         pitching_team = ifelse(inning_topbot == "Top", home_team, away_team),
         xwOBA = ifelse(is.na(estimated_woba_using_speedangle), woba_value, estimated_woba_using_speedangle),
         xBA = ifelse(is.na(estimated_ba_using_speedangle), 0, estimated_ba_using_speedangle),
         zone_whiff = ifelse(whiff == 1 & actual_strike == 1, 1, 0),
         runners_on = ifelse(!is.na(on_1b) | !is.na(on_2b) | !is.na(on_3b), 1, 0),
         risp = ifelse(!is.na(on_2b) | !is.na(on_3b), 1, 0),
         spray_angle = round(atan((hc_x-125.42)/(198.27-hc_y))*180/pi*.75, 1),
         field_hit = ifelse(spray_angle < -12.5, "left", ifelse(spray_angle > 12.5, "right", "center")),
         hit_type = case_when(
           field_hit == "left" & stand == "R" ~ "pull",
           field_hit == "center" & stand == "R" ~ "center",
           field_hit == "right" & stand == "R" ~ "oppo",
           field_hit == "left" & stand == "L" ~ "oppo",
           field_hit == "center" & stand == "L" ~ "center",
           field_hit == "right" & stand == "L" ~ "pull"
         ),
         sweet_spot = ifelse(launch_angle >= 8 & launch_angle <= 32, 1, 0),
         pitch_group = case_when(
           pitch_name %in% c("Sweeper", "Curveball", "Slider", "Knucke Curve", "Slurve", "Eephus", "Slow Curve", "Knuckleball") ~ "breaking",
           pitch_name %in% c("4-Seam Fastball", "Sinker", "Cutter") ~ "fastball",
           pitch_name %in% c("Split-Finger", "Forkball", "Screwball", "Changeup") ~ "offspeed"
         ),
         vy_f = -sqrt(vy0^2 - (2 * ay * (0 - plate_z))), # Assuming y0 is release_pos_z and yf is plate_z. If not, replace 0 with the correct value for yf.
         t = (vy_f - vy0) / ay,
         vz_f = vz0 + (az * t),
         VAA = -atan(vz_f / vy_f) * (180 / pi),
         dx = plate_x - release_pos_x, # Plate x - release x
         # 2. Calculate time to plate (t) - using vx0 (horizontal velocity)
         t_haa = dx / vx0, # Time = distance / velocity
         # 3. Calculate final horizontal velocity (vxf)
         vxf = vx0 + (ax * t_haa),
         # 4. Calculate Horizontal Approach Angle (HAA)
         HAA = atan(vz_f / vxf) * (180 / pi),
         ivb = (-16 * t^2 / 2) - (release_pos_z - plate_z),  # Gravity drop minus actual drop
         ext_velocity = sqrt(vx0^2 + vy0^2 + vz0^2),  # Total release velocity
         perceived_velocity = ext_velocity * (60.5 / (60.5 - release_extension)),
         expected_chase = case_when(
           pitch_group == "breaking" & actual_strike == 0 ~ 0.45,  # Sliders outside
           pitch_group == "fastball" & actual_strike == 0 ~ 0.35,  # Fastballs high
           pitch_group == "offspeed" & actual_strike == 0 ~ 0.40,  # Changeups low
           TRUE ~ 0.30
         ),
         edge = ifelse(zone %in% c(11, 12, 13, 14, 16, 17, 18, 19), 1, 0)) %>% 
  drop_na(actual_strike) -> data2024

data2024 <- data2024[!duplicated(data2024),]

# Save final dataset
saveRDS(data2024, file = "statcast_2024.rds")



## filter for starters
starter_data <- data2024 %>% 
  group_by(pitcher_name) %>% 
  filter(n()>1500) %>% 
  summarise(whiff_rate = mean(whiff[swing==T], na.rm=T),
            barrel_rate = mean(barrel[bb_type!='' & description!='foul'], na.rm=T),
            exit_velo = mean(launch_speed[bb_type!='' & description!='foul'], na.rm=T),
            fastball_velo = mean(release_speed[pitch_group == "fastball"], na.rm=T),
            sweet_spot_rate = mean(sweet_spot[bb_type!='' & description!='foul'], na.rm = T),
            xBA = mean(xBA[events!=''], na.rm = T),
            xwOBA = mean(xwOBA, na.rm=T),
            k_rate = mean(events[events!=''] == "strikeout", na.rm=T),
            bb_rate = mean(events[events!=''] == "walk", na.rm=T),
            hard_hit_rate = mean(hard_hit[bb_type != ''], na.rm=T),
            groundball_rate = mean(bb_type[bb_type!=''] == "ground_ball", na.rm = T),
            extension = mean(release_extension, na.rm=T),
            edge_rate = mean(edge, na.rm=T),
            expected_chase = mean(expected_chase, na.rm = T),
            chase_rate = mean(chase[actual_strike==F], na.rm=T),
            strike_pct = mean(actual_strike, na.rm=T))

### k-means
df_numeric <- starter_data %>% 
  select(c(chase_rate, whiff_rate, edge_rate, strike_pct, barrel_rate))

# Standardize the data
df_scaled <- scale(df_numeric)

# Determine optimal clusters using the elbow method
wss <- map_dbl(1:10, ~kmeans(df_scaled, .x, nstart = 25)$tot.withinss)

# Plot elbow method
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters", ylab = "Total Within-Cluster Sum of Squares")

# Perform K-means clustering with chosen k (e.g., k = 3)
set.seed(123)  # Ensure reproducibility
kmeans_result <- kmeans(df_scaled, centers = 2, nstart = 25)

# Add cluster assignments to the original data
starter_data$cluster <- as.factor(kmeans_result$cluster)

# View results
starter_data %>% arrange(cluster)

##plot
pca_result <- prcomp(df_scaled, center = TRUE, scale. = TRUE)

# Convert PCA results into a tibble
pca_data <- as_tibble(pca_result$x) %>%
  select(PC1, PC2) %>%  # Take first two components
  mutate(cluster = as.factor(kmeans_result$cluster),
         pitcher_name = starter_data$pitcher_name)

ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster, label = pitcher_name)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "K-Means Clustering of Pitchers (PCA Projection)",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  scale_color_manual(values = c("red", "blue", "green", "orange")) +
  geom_text(vjust = 1.5, size = 3)


starter_data %>% 
  group_by(cluster) %>% 
  summarise_all("mean") %>% 
  data.table::data.table()

View(starter_data)

#### add in minor league data
minor_leagues <- readRDS('/Users/matthew/Library/Mobile Documents/com~apple~CloudDocs/Documents/milb_pitch_data.rds')


minor_league_nums <- minor_leagues %>% 
  group_by(pitcher_name) %>% 
  filter(n()<1500 & n()>700) %>% 
  summarise(whiff_rate = mean(whiff[swing==T], na.rm=T),
            barrel_rate = mean(barrel[bb_type!='' & description!='foul'], na.rm=T),
            exit_velo = mean(launch_speed[bb_type!='' & description!='foul'], na.rm=T),
            fastball_velo = mean(release_speed[pitch_group == "fastball"], na.rm=T),
            sweet_spot_rate = mean(sweet_spot[bb_type!='' & description!='foul'], na.rm = T),
            k_rate = mean(result.eventType[last.pitch.of.ab=="true"] == "strikeout", na.rm=T),
            bb_rate = mean(result.eventType[last.pitch.of.ab=="true"] == "walk", na.rm=T),
            hard_hit_rate = mean(hard_hit[bb_type != ''], na.rm=T),
            groundball_rate = mean(bb_type[bb_type!=''] == "ground_ball", na.rm = T),
            extension = mean(release_extension, na.rm=T),
            edge_rate = mean(edge, na.rm=T),
            expected_chase = mean(expected_chase, na.rm = T),
            chase_rate = mean(chase[actual_strike==F], na.rm=T),
            strike_pct = mean(actual_strike, na.rm=T)) %>% 
  filter(!is.na(exit_velo))

### select numeric columns
minor_league_nums_df <- minor_league_nums %>% 
  select(c(chase_rate, whiff_rate, hard_hit_rate, groundball_rate))

## scale
df_minor_scaled <- scale(minor_league_nums_df)

## predict
# Compute distances between minor league data and k-means centroids
distances <- as.matrix(dist(rbind(kmeans_result$centers, df_minor_scaled)))[-(1:nrow(kmeans_result$centers)), 1:nrow(kmeans_result$centers)]

# Assign each minor league player to the closest cluster
minor_league_clusters <- apply(distances, 1, which.min)

### add clusters
minor_league_nums <- minor_league_nums %>%
  mutate(cluster = minor_league_clusters)


minor_league_nums %>% 
  group_by(cluster) %>% 
  summarise_all("mean") %>% 
  data.table::data.table()


## jays pitchers
jays_pitchers <- minor_leagues %>% 
  filter(pitching_team %in% c("Buffalo Bisons", 
                              "New Hampshire Fisher Cats")) %>% 
  pull(pitcher_name) %>% unique()


minor_league_nums %>%
  filter(pitcher_name %in% jays_pitchers) %>% 
  View


### pull free agent pitchers
free_agents <- c("Andrew Heaney", "David Robertson", "Patrick Corbin", 
                 "Kyle Gibson", "Chris Flexen", "Lance Lynn", "Ross Stripling",
                 "Jose Quintana", "Spencer Turnbull", "Jalen Beeks", "Dylan Floro", 
                 "Adam Ottavino", "Andrew Chafin", "Brooks Raley", "Hunter Strickland",
                 "Marco Gonzales", "Dillon Tate", "Yency Almonte", "Craig Kimbrel", 
                 "Matt Barnes", "Phil Maton", "José Ureña", "Scott Alexander", 
                 "Jordan Lyles", "Héctor Neris", "Kyle Finnegan", "Adam Cimber",
                 "Joe Kelly", "Ryan Yarbrough", "Alex Wood", "Joely Rodríguez", 
                 "Mike Clevinger", "Shelby Miller", "José Cisnero", "Jay Jackson", 
                 "Drew Smyly", "Lucas Sims", "Will Smith","Matt Moore", 
                 "Anthony DeSclafani", "Noah Syndergaard", "Keynan Middleton", 
                 "Trevor Gott", "Dylan Covey", "José Urquidy", "Daniel Bard")


starter_data %>% filter(pitcher_name%in% free_agents) %>% View

## this is what shows up on statcast page
savant_data <- data2024 %>% 
  group_by(pitcher_name) %>% 
  mutate(reliever = n()<1500) %>% 
  group_by(pitcher_name, reliever) %>% 
  filter(n()>150) %>%
  summarise(whiff_rate = mean(whiff[swing==T], na.rm=T),
            barrel_rate = mean(barrel[bb_type!='' & description!='foul'], na.rm=T),
            exit_velo = mean(launch_speed[bb_type!='' & description!='foul'], na.rm=T),
            fastball_velo = mean(release_speed[pitch_group == "fastball"], na.rm=T),
            sweet_spot_rate = mean(sweet_spot[bb_type!='' & description!='foul'], na.rm = T),
            xBA = mean(xBA[events!=''], na.rm = T),
            xwOBA = mean(xwOBA, na.rm=T),
            k_rate = mean(events[events!=''] == "strikeout", na.rm=T),
            bb_rate = mean(events[events!=''] == "walk", na.rm=T),
            hard_hit_rate = mean(hard_hit[bb_type != ''], na.rm=T),
            groundball_rate = mean(bb_type[bb_type!=''] == "ground_ball", na.rm = T),
            extension = mean(release_extension, na.rm=T),
            edge_rate = mean(edge, na.rm=T),
            expected_chase = mean(expected_chase, na.rm = T),
            chase_rate = mean(chase[actual_strike==F], na.rm=T),
            strike_pct = mean(actual_strike, na.rm=T)) %>% 
  ungroup()



### movement stats (fastball h movement, breaking v movement, spin)
fastball_data <- data2024 %>% 
  filter(pitch_group=='fastball') %>% 
  group_by(pitcher_name) %>% 
  mutate(reliever = n()<1500) %>% 
  group_by(pitcher_name, reliever) %>% 
  filter(n()>150) %>%
  summarise(fastball_velo = mean(release_speed, na.rm=T),
            fastball_movement = abs(mean(pfx_x, na.rm = T)),
            VAA = mean(VAA, na.rm = T),
            fastball_spin = mean(release_spin_rate, na.rm = T),
            HAA = mean(HAA, na.rm = T),
            release_pos = mean(release_pos_z, na.rm=T)) %>% 
  ungroup()


starter_data <- savant_data %>% 
  filter(reliever==T) 

### k-means
df_numeric <- starter_data %>% 
  select(c(whiff_rate, barrel_rate, groundball_rate, fastball_velo, k_rate, bb_rate, 
           extension, sweet_spot_rate, xwOBA, exit_velo))

# Standardize the data
df_scaled <- scale(df_numeric)

# add rownames
rownames(df_scaled) <- starter_data$pitcher_name

# Determine optimal clusters using the elbow method
wss <- map_dbl(1:10, ~kmeans(df_scaled, .x, nstart = 25)$tot.withinss)

# Plot elbow method
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters", ylab = "Total Within-Cluster Sum of Squares")

# Perform K-means clustering with chosen k (e.g., k = 3)
set.seed(123)  # Ensure reproducibility
kmeans_result <- kmeans(df_scaled, centers = 5, nstart = 25)

### plot
fviz_cluster(kmeans_result, data = df_scaled)


# Add cluster assignments to the original data
starter_data$cluster <- as.factor(kmeans_result$cluster)


# group by cluster, get means
starter_data %>% 
  group_by(cluster) %>% 
  summarise_all("mean") %>% 
  data.table::data.table()


# View results
starter_data %>% filter(pitcher_name %in% free_agents) %>% View

##plot
pca_result <- prcomp(df_scaled, center = TRUE, scale. = TRUE)

# Convert PCA results into a tibble
pca_data <- as_tibble(pca_result$x) %>%
  select(PC1, PC2) %>%  # Take first two components
  mutate(cluster = as.factor(kmeans_result$cluster),
         pitcher_name = starter_data$pitcher_name)

ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster, label = pitcher_name)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "K-Means Clustering of Pitchers (PCA Projection)",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  scale_color_manual(values = c("red", "blue", "green", "orange")) +
  geom_text(vjust = 1.5, size = 3)


fviz_cluster()

