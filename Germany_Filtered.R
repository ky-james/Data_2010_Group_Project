library(tidyverse)
library(ggplot2)

# Section 1: Reading Data
matches = read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/wcmatches.csv")
world_cups = read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv")
 
# Section 2: Filtering Matches
unified_germany_games = matches %>% filter(home_team %in% c("Germany") | away_team %in% c("Germany"))
split_germany_games = matches %>% filter(home_team %in% c("West Germany", "East Germany") | away_team %in% c("West Germany", "East Germany"))
west_germany_games = split_germany_games %>% filter(home_team %in% c("West Germany") | away_team %in% c("West Germany"))
east_germany_games = split_germany_games %>% filter(home_team %in% c("East Germany") | away_team %in% c("East Germany"))

# filtering home & away matches
unified_germany_home_games = unified_germany_games %>% filter(home_team == "Germany")
unified_germany_away_games = unified_germany_games %>% filter(away_team == "Germany")
split_germany_home_games = split_germany_games %>% filter(home_team %in% c("West Germany", "East Germany"))
split_germany_away_games = split_germany_games %>% filter(away_team %in% c("West Germany", "East Germany"))
west_germany_home_games = split_germany_games %>% filter(home_team == "West Germany")
west_germany_away_games = split_germany_games %>% filter(away_team == "West Germany")
east_germany_home_games = split_germany_games %>% filter(home_team == "East Germany")
east_germany_away_games = split_germany_games %>% filter(away_team == "East Germany")

# filtering wins
unified_germany_wins = unified_germany_games %>% filter(winning_team %in% c("Germany"))
split_germany_wins = split_germany_games %>% filter(winning_team %in% c("West Germany", "East Germany"))
west_germany_wins = split_germany_wins %>% filter(winning_team %in% c("West Germany"))
east_germany_wins = split_germany_wins %>% filter(winning_team %in% c("East Germany"))

# filtering losses
unified_germany_losses = unified_germany_games %>% filter(losing_team %in% c("Germany"))
split_germany_losses = split_germany_games %>% filter(losing_team %in% c("West Germany", "East Germany"))
west_germany_losses = split_germany_losses %>% filter(losing_team %in% c("West Germany"))
east_germany_losses = split_germany_losses %>% filter(losing_team %in% c("East Germany"))

# filtering draws
unified_germany_draws = unified_germany_games %>% filter(winning_team %in% c(NA))
split_germany_draws = split_germany_games %>% filter(winning_team %in% c(NA))
west_germany_draws = west_germany_games %>% filter(winning_team %in% c(NA))
east_germany_draws = east_germany_games %>%  filter(winning_team %in% c(NA))

# filtering goals scored
# to calculate the goals scored for each German team, we will sum the goals of home_score and away_score for germanTeam_home_games and germanTeam_away_games
unified_germany_goals_for = sum(sum(unified_germany_home_games$home_score) + sum(unified_germany_away_games$away_score))
split_germany_goals_for = sum((sum(west_germany_home_games$home_score) + sum(west_germany_away_games$away_score)) + (sum(east_germany_home_games$home_score) + sum(east_germany_away_games$away_score)))
west_germany_goals_for = sum(sum(west_germany_home_games$home_score) + sum(west_germany_away_games$away_score))
east_germany_goals_for = sum(sum(east_germany_home_games$home_score) + sum(east_germany_away_games$away_score))

# fitlering goals against
# to caclulate the goals agianst for each Germany team, we will sum the goals of home_score and away_score for germanTeams_away_games and germanyTeam_home_teams
unified_germany_goals_against = sum(sum(unified_germany_home_games$away_score) + sum(unified_germany_away_games$home_score))
split_germany_goals_against = sum((sum(west_germany_home_games$away_score) + sum(west_germany_away_games$home_score)) + (sum(east_germany_home_games$away_score) + sum(east_germany_away_games$home_score)))
west_germany_goals_against = sum(sum(west_germany_home_games$away_score) + sum(west_germany_away_games$home_score))
east_germany_goals_against = sum(sum(east_germany_home_games$away_score) + sum(east_germany_away_games$home_score))

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

unified_germany_goals_for
unified_germany_goals_against
split_germany_goals_for
split_germany_goals_against
west_germany_goals_for
west_germany_goals_against
east_germany_goals_for
east_germany_goals_against

# Section 4: Conclusions
# Unified Germany: 47GP - 32W - 10L - 5D - 68.1W% - 95GF - 77GA
# Split Germany: 68GP - 41W - 14L - 13D - 60.3W% - 136GF - 82GA
# West Germany: 68GP - 39W - 13L - 11D - 62.9W% - 131GF - 77GA
# East Germany: 6GP - 2W - 2L - 2D - 33.3W% - 5GF - 5GA

# Section 5: Data Visualization
# # stacked bar plot for each German team's wins, losses, and draws
germany_game_data = data.frame(
  team = c("Unified Germany", "West Germany", "East Germany"),
  wins = c(num_unified_germany_wins, num_west_germany_wins, num_east_germany_wins),
  draws = c(num_unified_germany_draws, num_west_germany_draws, num_east_germany_draws),
  losses = c(num_unified_germany_losses, num_west_germany_losses, num_east_germany_losses),
  games_played = c(num_unified_germany_games, num_west_germany_games, num_east_germany_games))

germany_game_data_long= germany_game_data %>% 
  pivot_longer(cols = c("wins", "losses", "draws"), names_to = "game_result", values_to = "occurences")

ggplot(germany_game_data_long, aes(x = team, y = occurences, fill = game_result)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Team", y = "Number of Games") +
  ggtitle("Game Result by Germany Team") + 
  scale_fill_manual(values = c("#FF6B6B", "#FFE66D", "#6BD4FF"), 
                    name = "Game Result",
                    labels = c("Draw", "Loss", "Win"))

# grouped bar chart for each German team's goals for and goals against
germany_goals_data = data.frame(team = c("United Germany", "West Germany", "East Germany"),
                               goals_for = c(unified_germany_goals_for, west_germany_goals_for, east_germany_goals_for),
                               goals_against = c(unified_germany_goals_against, west_germany_goals_against, east_germany_goals_against))

germany_goals_data_long = german_goals_data %>% pivot_longer(cols =c("goals_for", "goals_against"))

ggplot(german_goals_data_long, aes(fill = name, y=value, x = team)) + 
  geom_bar(position="dodge", stat="identity") +
  labs(x = "Team", y = "Number of Goals") + 
  ggtitle("Goals For and Goals Against by German Team") + 
  scale_fill_manual(values = c("#FF6B6B", "#6BD4FF"), 
                    name = "Goal Type",
                    labels = c("Goals For", "Goals Against"))








