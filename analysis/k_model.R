library(tidyverse)

## filter for 2023
data2023 <- data_pitches %>% 
  filter(game_date <= "2023-12-31")

## take daily averages on a team level
data2023.grouped <- data2023 %>% 
  group_by(game_date, hitting_team) %>% 
  summarise(whiff_pct = mean(whiff, na.rm = T),
            chase_pct = mean(chase, na.rm = T),
            barrel_pct = mean(barrel, na.rm = T)) %>% 
  ungroup()

## create a rolling average for the 3 variables above
data2023.rolling <- data2023.grouped %>% 
  arrange(game_date) %>% 
  group_by(hitting_team) %>% 
  mutate(roll_whiff = lag(zoo::rollmean(whiff_pct, k = 5, fill = NA, align = "right"), 1),
         roll_chase = lag(zoo::rollmean(chase_pct, k = 5, fill = NA, align = "right"), 1),
         roll_barrel = lag(zoo::rollmean(barrel_pct, k = 5, fill = NA, align = "right"), 1))


## create game logs for player
pitcher.game_logs <- data_pitches %>% 
  distinct(balls, strikes, inning, release_pos_x, release_pos_z, release_speed,
           .keep_all = T) %>% 
  filter(events != "") %>%
  group_by(pitcher_name, game_date, hitting_team) %>% 
  summarise(ip = max(inning) - min(inning) + 1,
            k = sum(events == "strikeout"),
            bb = sum(events == "walk"),
            start = min(inning) == 1,
            whiff = mean(whiff),
            chase = mean(chase)) %>% 
  ungroup() %>% 
  arrange(game_date) %>% 
  group_by(pitcher_name) %>% 
  mutate(rolling_whiff_rate = lag(zoo::rollmean(whiff, k = 5, fill = NA, align = "right"), 1),
         rolling_chase_rate = lag(zoo::rollmean(chase, k = 5, fill = NA, align = "right"), 1)) %>% 
  ungroup()


## merge pitcher data with rolling stats
pitcher.game_logs <- pitcher.game_logs %>% 
  left_join(data2023.rolling,
            by = c("game_date",
                   "hitting_team")) %>% 
  filter(!is.na(roll_whiff)) %>% 
  filter(!is.na(rolling_whiff_rate))


## filter for starters only
starter.game_logs <- pitcher.game_logs %>% 
  filter(start == T,
         ip > 2)


starter.game_logs_lines <- starter.game_logs %>% 
  merge(k_lines, by.y = c("Player", "Date"), by.x = c("pitcher_name", "game_date"))


write_csv(starter.game_logs_lines %>% 
            select(-c(ip, bb, whiff, chase,
                      whiff_pct, chase_pct, barrel_pct)), 'starter_gamelogs.csv')

## random forest
library(randomForest)


starter.game_logs_lines <- starter.game_logs_lines %>% 
  mutate(over = k > SO_Prop)

starter.game_logs_lines$over <- as.factor(starter.game_logs_lines$over)

set.seed(21)
train_split <- initial_split(starter.game_logs_lines)
train_data <- training(train_split)
test_data <- testing(train_split)
test_data <- test_data %>% filter(pitcher_name %in% train_data$pitcher_name)

so_model <- randomForest(over ~ roll_whiff + roll_chase + roll_barrel + SO_Prop +
               rolling_chase_rate + rolling_whiff_rate,
             data = train_data)

print(so_model)

predictions <- predict(so_model, test_data)
confusionMatrix <- table(predictions, test_data$over)
print(confusionMatrix)

caret::confusionMatrix(predictions, test_data$over,
                       positive = "TRUE")




# Define the control parameters for the training process, including cross-validation
trainControl <- trainControl(method = "cv", number = 10)

# Set up a grid of hyperparameters to search over
tuneGrid <- expand.grid(.mtry = c(2, 3, 4, 5, 6, 7, 8, 9, 10))

# Train the model with hyperparameter tuning
model <- train(over ~ roll_whiff + roll_chase + roll_barrel + SO_Prop +
                 rolling_chase_rate + rolling_whiff_rate,
               data = train_data,
               method = "rf",
               trControl = trainControl,
               tuneGrid = tuneGrid,
               metric = "Accuracy")


predictions <- predict(model, test_data)





## create lmer model
library(lme4)
library(tidymodels)

## split into train/test set
set.seed(21)
train_split <- initial_split(starter.game_logs_lines)
train_data <- training(train_split)
test_data <- testing(train_split)
test_data <- test_data %>% filter(pitcher_name %in% train_data$pitcher_name)

null_model <- lmer(k ~ (1|pitcher_name),
     data = train_data)

sjPlot::tab_model(null_model)  

hit_stat.mod <- lm(k ~ roll_chase + rolling_whiff_rate + rolling_chase_rate +
                       roll_whiff + SO_Prop,
     data = train_data)


hit_stat.lm_mod <- lmer(k ~ (1|pitcher_name) + SO_Prop,
                     data = train_data)

sjPlot::tab_model(hit_stat.mod)


test_data$estimate <- predict(hit_stat.mod, newdata = test_data)
test_data$null_estimate <- predict(null_model, newdata = test_data)

plot(test_data$k, test_data$estimate)

plot(hit_stat.mod)


test_data %>% 
  rmse(estimate = estimate, truth = k)


test_data %>% 
  mutate(over_pred = estimate>SO_Prop, 
         over = k>SO_Prop, 
         correct = over_pred == over) %>% 
  pull(correct) %>% 
  mean()


write.csv()

## xg boost
library(xgboost)

xgtrain <- train_data %>% 
  select(k, roll_whiff, roll_chase, roll_barrel, rolling_whiff_rate, rolling_chase_rate,
         SO_Prop)

data.table::setDT(xgtrain)
data.table::setDT(test_data)

train_matrix <- xgb.DMatrix(data = as.matrix(xgtrain %>% select(-k)), label = xgtrain[, k])
test_matrix <- xgb.DMatrix(data = as.matrix(test_data %>% select(roll_whiff, roll_chase, roll_barrel, rolling_whiff_rate, rolling_chase_rate,
                                                                 SO_Prop)), label = test_data[, k])


# Define parameters
params <- list(
  objective = "reg:squarederror",  # for regression task
  booster = "gbtree",  # gradient boosted tree
  eta = 0.1,  # learning rate
  max_depth = 6,  # maximum depth of a tree
  nrounds = 100,  # number of boosting rounds
  eval_metric = "rmse"  # evaluation metric
)

# Train the model
xgb_model <- xgboost(params = params, data = train_matrix, nrounds = params$nrounds)

pred <- predict(xgb_model, test_matrix)

# For regression tasks
RMSE <- sqrt(mean((test_data[, k] - pred)^2))


test_data$pred <- pred

plot(test_data$pred, test_data$k)


# Define parameter grid
param_grid <- list(
  eta = c(0.01, 0.05, 0.1),
  max_depth = c(3, 6, 9),
  gamma = c(0, 0.1, 0.2),
  subsample = c(0.7, 0.8, 0.9),
  colsample_bytree = c(0.7, 0.8, 0.9)
)

# Perform grid search with cross-validation
xgb_grid <- xgb.cv(
  params = list(
    objective = "reg:linear",
    booster = "gbtree"
  ),
  data = train_matrix,
  nrounds = 100,
  nfold = 5,  # 5-fold cross-validation
  early_stopping_rounds = 10,  # stop if no improvement in 10 rounds
  verbose = TRUE,
  grid = param_grid
)

# Extract the best parameters
best_params <- xgb_grid$params

# Train the final model using the best parameters
final_model <- xgboost(
  params = best_params,
  data = train_matrix,
  nrounds = 100
)

# Evaluate on the test set
predictions <- predict(final_model, test_matrix)


RMSE <- sqrt(mean((test_data[, k] - predictions)^2))

test_data$pred <- predictions

plot(test_data$k, predictions)


