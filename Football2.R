# Load necessary libraries
library(nflreadr)
library(dplyr)
library(tidyr)
library(xgboost)
library(ggplot2)
library(Metrics)
library(utils)

# Load play-by-play participation data for the specified seasons
pbp <- load_participation(seasons = 2016:2023, include_pbp = TRUE)

#Print column names in pbp
colnames(pbp)

# Filter the dataset for specific quarterback actions (dropbacks without scrambles or sacks)
pbp <- pbp %>%
  filter(qb_dropback == 1 & qb_scramble == 0 & sack == 0)

# Create a binary column for pressure situations
pbpclean <- pbp %>%
  mutate(pressure = ifelse(was_pressure == "TRUE", 1, 0))

# Select relevant columns for further analysis
selected <- pbpclean %>%
  select(passer_player_name, posteam, season, week, complete_pass, pressure, 
         shotgun, ngs_air_yards, time_to_throw, down, ydstogo, yardline_100, pass_location)

# Remove rows with NA values
pbpselected <- na.omit(selected)

# Convert specified columns to factors for modeling
pbpselected <- pbpselected %>%
  mutate(
    down = as.factor(down), 
    pass_location = as.factor(pass_location), 
    shotgun = as.factor(shotgun), 
    pressure = as.factor(pressure)
  )

# Create indicator variables for down using pivot_wider
pbp_down_encoded <- pbpselected %>%
  mutate(down_indicator = 1) %>%
  pivot_wider(names_from = down, values_from = down_indicator, values_fill = list(down_indicator = 0), names_prefix = "down_")

#View data
pbp_down_encoded

# Create indicator variables for pass location
pbp_pass_loc_encoded <- pbp_down_encoded %>%
  mutate(loc_indicator = 1) %>%
  pivot_wider(names_from = pass_location, values_from = loc_indicator, values_fill = list(loc_indicator = 0), names_prefix = "pass_loc_")

# View the encoded data
pbp_pass_loc_encoded

# Create indicator variables for shotgun formation
pbp_shotgun_encoded <- pbp_pass_loc_encoded %>%
  mutate(shotgun_indicator = 1) %>%
  pivot_wider(names_from = shotgun, values_from = shotgun_indicator, values_fill = list(shotgun_indicator = 0), names_prefix = "shotgun_")

# View the encoded data
pbp_shotgun_encoded

# Create indicator variables for pressure situations
pbp_pressure_encoded <- pbp_shotgun_encoded %>%
  mutate(pressure_indicator = 1) %>%
  pivot_wider(names_from = pressure, values_from = pressure_indicator, values_fill = list(pressure_indicator = 0), names_prefix = "pressure_")

# View the encoded data
pbp_pressure_encoded

# Prepare training and test datasets
finaldata <- pbp_pressure_encoded
train_data <- finaldata %>%
  filter(season <= 2021)

test_data <- finaldata %>%
  filter(season > 2021)

# Set a seed for reproducibility
set.seed(2015)

# Create DMatrix for training data
train_matrix <- xgb.DMatrix(data = as.matrix(train_data %>% select(-passer_player_name, -season, -week, -posteam)),
                            label = train_data$complete_pass)

# Define parameters for the XGBoost model
params <- list(
  objective = "binary:logistic",  # Corrected the objective
  eval_metric = "logloss",
  max_depth = 6,
  eta = 0.3
)

# Train the XGBoost model
xgboost_model <- xgboost(
  data = train_matrix,
  params = params,
  nrounds = 100
)

# Create DMatrix for test data
test_matrix <- xgb.DMatrix(data = as.matrix(test_data %>% select(-passer_player_name, -season, -week, -posteam)))

# Make predictions on the test dataset
test_data$predicted_complete_pass <- predict(xgboost_model, test_matrix)

# Calculate Brier score for predictions
brier <- mean((test_data$predicted_complete_pass - test_data$complete_pass)^2)

# Calculate the difference between actual and expected complete passes
results <- test_data %>%
  mutate(cpoe = complete_pass - predicted_complete_pass)

# Summarize the results by passer name and season
eval <- results %>%
  group_by(passer_player_name, season) %>%
  summarize(
    att = n(),  # Number of attempts
    exp = mean(predicted_complete_pass),  # Expected completions
    act = mean(complete_pass),  # Actual completions
    cpoe = mean(cpoe)  # Completion percentage over expectation
  ) %>%
  filter(att >= 150)  # Filter for players with at least 150 attempts

# Filter data for Patrick Mahomes for specified seasons
mahomes_data <- test_data %>%
  filter(passer_player_name == "P.Mahomes", season %in% c(2022, 2023)) %>%  # Use 'c()' for multiple values
  mutate(
    air_yard_bin = cut(ngs_air_yards, breaks = seq(-10, 50, by = 5))  # Bin air yards
  ) %>%
  group_by(passer_player_name, air_yard_bin, season) %>%
  summarise(cpoe = mean(complete_pass - predicted_complete_pass, na.rm = TRUE))  # Calculate mean cpoe

# Create a plot of cpoe against air yards for Mahomes
ggplot(mahomes_data, aes(x = air_yard_bin, y = cpoe, color = factor(season))) +
  geom_line() +  # Add lines for each season
  labs(title = "CPOE by Air Yards for P. Mahomes",
       x = "Air Yards Binned",
       y = "Completion Percentage Over Expectation (CPOE)",
       color = "Season") +
  theme_minimal()  # Use a minimal theme for clarity



