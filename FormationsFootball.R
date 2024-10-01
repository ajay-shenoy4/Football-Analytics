# Load necessary libraries
library(nflreadr)
library(dplyr)
library(tidyr)
library(xgboost)
library(ggplot2)
library(Metrics)
library(utils)

# Load play-by-play participation data for the specified seasons
pbp_new <- load_participation(seasons = 2016:2023, include_pbp = TRUE)

# Filter the dataset for only pass and run play types
pbp_pass_run <- pbp_new %>%
  filter(play_type %in% c("pass", "run"))

# Create bins for 'ydstogo'
pbp_binned <- pbp_pass_run %>%
  mutate(ydstogo_group = case_when(
    ydstogo >= 20 ~ "20+",
    ydstogo >= 15 ~ "15+",
    ydstogo >= 12 ~ "12+",
    ydstogo >= 10 ~ "10+",
    ydstogo >= 8  ~ "8+",
    ydstogo >= 6 ~ "6+",
    ydstogo >= 5  ~ "5+",
    ydstogo >= 3  ~ "3+",
    ydstogo >= 2  ~ "2+",
    ydstogo >= 1 ~ "1+",
    ydstogo < 1   ~ "<1"
  ))

# Filter for pass plays and group by down, ydstogo_group, and offense_formation
pbp_pass <- pbp_binned %>%
  filter(play_type == "pass") %>%
  group_by(down, ydstogo_group, offense_formation) %>%
  summarize(
    total_plays = n(),                             # Total number of pass plays
    mean_defenders_in_box = mean(defenders_in_box, na.rm = TRUE),  # Average defenders in the box
    avg_yards_gained = mean(yards_gained, na.rm = TRUE),           # Average yards gained
    mean_time_to_throw = mean(time_to_throw, na.rm = TRUE),        # Average time to throw
    mean_was_pressure = mean(was_pressure, na.rm = TRUE),          # Average pressure
    mean_ydstogo = mean(ydstogo, na.rm = TRUE),                    # Average ydstogo
    passes_with_greater_yards_gained = sum(yards_gained > ydstogo, na.rm = TRUE), # Count passes with yards_gained > ydstogo
    percentage_passes_greater_yards_gained = (passes_with_greater_yards_gained / total_plays) * 100  # Percentage
  )

# Filter for run plays and group by down, ydstogo_group, and offense_formation
pbp_run <- pbp_binned %>%
  filter(play_type == "run") %>%
  group_by(down, ydstogo_group, offense_formation) %>%
  summarize(
    total_plays = n(),                             # Total number of run plays
    mean_defenders_in_box = mean(defenders_in_box, na.rm = TRUE),  # Average defenders in the box
    avg_yards_gained = mean(yards_gained, na.rm = TRUE),           # Average yards gained
    mean_time_to_throw = mean(time_to_throw, na.rm = TRUE),        # Average time to throw (irrelevant for run)
    mean_was_pressure = mean(was_pressure, na.rm = TRUE),          # Average pressure (if applicable)
    mean_ydstogo = mean(ydstogo, na.rm = TRUE),                    # Average ydstogo
    runs_with_greater_yards_gained = sum(yards_gained > ydstogo, na.rm = TRUE),  # Count runs with yards_gained > ydstogo
    percentage_runs_greater_yards_gained = (runs_with_greater_yards_gained / total_plays) * 100  # Percentage
  )

# Combine pass and run data
pbp_combined <- bind_rows(
  pbp_pass %>% mutate(play_type = "pass"),
  pbp_run %>% mutate(play_type = "run")
) %>%
  # Combine 'passes_with_greater_yards_gained' and 'runs_with_greater_yards_gained' into a single column
  mutate(greater_yards_gained_count = coalesce(passes_with_greater_yards_gained, runs_with_greater_yards_gained)) %>%
  select(-passes_with_greater_yards_gained, -runs_with_greater_yards_gained) %>%
  # Combine 'percentage_passes_greater_yards_gained' and 'percentage_runs_greater_yards_gained' into a single column
  mutate(percentage_greater_yards_gained = coalesce(percentage_passes_greater_yards_gained, percentage_runs_greater_yards_gained)) %>%
  select(-percentage_passes_greater_yards_gained, -percentage_runs_greater_yards_gained)


# Create the new filtered table
good_formations <- pbp_combined %>%
  # Filter rows based on conditions
  filter(percentage_greater_yards_gained > 50, total_plays > 5)

# Split the pbp_filtered dataframe into a list of dataframes, one for each down
pbp_by_down <- split(good_formations, good_formations$down)

# Example: Access the table for down 2
down_2_table <- pbp_by_down[[1]]

# Example: Access the table for down 3
down_3_table <- pbp_by_down[[2]]

# Example: Access the table for down 4
down_4_table <- pbp_by_down[[3]]

# Function to get user input and recommend an offense formation and play type
recommend_play <- function(pbp_combined) {
  # Ask the user for input
  defenders_input <- as.numeric(readline(prompt = "Enter the number of defenders in the box: "))
  down_input <- as.numeric(readline(prompt = "Enter the down (1, 2, 3, or 4): "))
  ydstogo_input <- as.numeric(readline(prompt = "Enter yards to go: "))
  
  # Create a filtered table based on the user's inputs and conditions
  recommendation <- pbp_combined %>%
    # Filter rows for the same down and yards to go group
    filter(down == down_input) %>%
    filter(
      mean_ydstogo >= ydstogo_input - 1.5 & mean_ydstogo <= ydstogo_input + 1.5,  # Match yardage within a reasonable range
      mean_defenders_in_box >= defenders_input - 1.5 &                           # Match defenders within 1.5 of user's input
        mean_defenders_in_box <= defenders_input + 1.5,
      total_plays > 5,                                                           # Ensure there are more than 5 plays
      !is.na(offense_formation)                                                  # Exclude rows with NA offense formation
    ) %>%
    # Sort by percentage_greater_yards_gained to get the best recommendation
    arrange(desc(percentage_greater_yards_gained)) %>%
    # Select only relevant columns for the recommendation
    select(offense_formation, play_type, percentage_greater_yards_gained) %>%
    # Get the top recommendation
    head(1)
  
  # Provide a recommendation if available
  if (nrow(recommendation) > 0) {
    print(paste("Recommended formation:", recommendation$offense_formation))
    print(paste("Recommended play type:", recommendation$play_type))
    print(paste("This play has a", recommendation$percentage_greater_yards_gained, "% chance of gaining more yards than needed."))
  } else {
    print("No recommendation found based on the provided inputs.")
  }
}

# Call the function to get a recommendation
recommend_play(pbp_combined)
