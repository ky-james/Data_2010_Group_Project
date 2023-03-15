library(tidyverse)

# Section 1: Reading Data
matches = read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/wcmatches.csv")
world_cups = read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv")
 
# Section 2: Filtering Matches
unified_germany_games = matches %>% filter(home_team %in% c("Germany") | away_team %in% c("Germany"))
split_germany_games = matches %>% filter(home_team %in% c("West Germany", "East Germany") | away_team %in% c("West Germany", "East Germany"))
west_germany_games = split_germany_games %>% filter(home_team %in% c("West Germany") | away_team %in% c("West Germany"))
east_germany_games = split_germany_games %>% filter(home_team %in% c("East Germany") | away_team %in% c("East Germany"))

unified_germany_wins = unified_germany_games %>% filter(winning_team %in% c("Germany"))
split_germany_wins = split_germany_games %>% filter(winning_team %in% c("West Germany", "East Germany"))
west_germany_wins = split_germany_wins %>% filter(winning_team %in% c("West Germany"))
east_germany_wins = split_germany_wins %>% filter(winning_team %in% c("East Germany"))

unified_germany_losses = unified_germany_games %>% filter(losing_team %in% c("Germany"))
split_germany_losses = split_germany_games %>% filter(losing_team %in% c("West Germany", "East Germany"))
west_germany_losses = split_germany_losses %>% filter(losing_team %in% c("West Germany"))
east_germany_losses = split_germany_losses %>% filter(losing_team %in% c("East Germany"))

unified_germany_draws = unified_germany_games %>% filter(winning_team %in% c(NA))
split_germany_draws = split_germany_games %>% filter(winning_team %in% c(NA))
west_germany_draws = west_germany_games %>% filter(winning_team %in% c(NA))
east_germany_draws = east_germany_games %>%  filter(winning_team %in% c(NA))

# Section 3: Analysis of Filtered Matches
num_unified_germany_games = nrow(unified_germany_games)
num_split_germany_games = nrow(split_germany_games) +1 
# TODO: remove the above +1
# This +1 needs to be added because West Germany played East Germany and the above line does not account for that
num_west_germany_games = nrow(west_germany_games)
num_east_germany_games = nrow(east_germany_games)

num_unified_germany_wins = nrow(unified_germany_wins)
num_split_germany_wins = nrow(split_germany_wins)
num_west_germany_wins = nrow(west_germany_wins)
num_east_germany_wins = nrow(east_germany_wins)

num_unified_germany_losses = nrow(unified_germany_losses)
num_split_germany_losses = nrow(split_germany_losses)
num_west_germany_losses = nrow(west_germany_losses)
num_east_germany_losses = nrow(east_germany_losses)

num_unified_germany_draws = nrow(unified_germany_draws)
num_split_germany_draws = nrow(split_germany_draws)
num_west_germany_draws = nrow(west_germany_draws)
num_east_germany_draws = nrow(east_germany_draws)

unified_germany_win_percent = num_unified_germany_wins / num_unified_germany_games
split_germany_win_percent = num_split_germany_wins / num_split_germany_games
west_germany_win_percent = num_west_germany_wins / num_west_germany_games
east_germany_win_percent= num_east_germany_wins / num_east_germany_games

# Section 4: Conclusions
# Unified Germany: 47GP - 32W - 10L - 5D - 68.1W%
# Split Germany: 68GP - 41W - 14L - 13D - 60.3W%
# West Germany: 68GP - 39W - 13L - 11D - 62.9W%
# East Germany: 6GP - 2W - 2L - 2D - 33.3W%



