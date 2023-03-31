World_cups = read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv")
Matches = read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/wcmatches.csv")

Matches[704,]$winning_team = "Portugal"
Matches[892,]$losing_team = "Colombia"

library(tidyverse)

World_cups %>% mutate(goals_per_game = goals_scored/games) %>% 
  ggplot(aes(x = year, y = goals_per_game)) +
  geom_point()

count = Matches %>% count(outcome)

# Rank teams based on goal differential
# Scoring system

Matches %>% filter(home_team == "Hungary")

# Get all the home goals
Home_Goals = Matches %>% group_by(home_team) %>% 
  summarise(home_goals = sum(home_score), H_goals_against = sum(away_score),
            H_total_matches = n()) %>% 
  rename(country = home_team)

# Get all the away goals
Away_Goals = Matches %>% group_by(away_team) %>% 
  summarise(away_goals = sum(away_score), A_goals_against = sum(home_score),
            A_total_matches = n()) %>% 
  rename(country = away_team)

# Combine into one dataset
Goals = Home_Goals %>% 
  full_join(Away_Goals, by = "country")

# Remove NA values
for(i in seq(1:length(Goals$country)))
{
  for(j in seq(1:length(Goals[i,])))
  {
    if(is.na(Goals[i,j]))
    {
      Goals[i,j] = 0
    }
  }
}

# Calculating additional stats for goals
Goals = Goals %>% 
  mutate(total_goals = home_goals + away_goals,
         total_goals_against = H_goals_against + A_goals_against,
         diff = total_goals - total_goals_against,
         total_matches = H_total_matches + A_total_matches,
         goals_per_game = total_goals/total_matches,
         goals_against_per_game = total_goals_against/total_matches)

# Plots
Goals %>% ggplot(aes(x = total_goals, y = total_goals_against,
                     label = country)) + geom_point() + 
  geom_text(hjust=0, vjust=-1, size = 2)

Goals %>% ggplot(aes(x = goals_per_game, y = goals_against_per_game,
                     label = country)) + geom_point() + 
  geom_text(hjust=0, vjust=-1, size = 2)

Goals %>% cor(Goals$goals_per_game, Goals$goals_against_per_game)
# Matched paired test?

# RENAME GROUP AND KNOCKOUT (You may want to remove the knockout part if you
# wish to have different levels of importance like semi-final vs final)
matches_rename = Matches

for(i in seq(1, length(matches_rename$stage)))
{
  if(matches_rename[i,]$stage %in% c("Group 1", "Group 2", "Group 3",
                                    "Group 4", "Group 5","Group 6", 
                                    "Group D ", " Group A",
                                    "Group A", "Group B", "Group C",
                                    "Group D", "Group E", "Group F",
                                    "Group G", "Group H", "Group I"))
  {
    matches_rename[i,]$stage = "Group"
  }
  
  if(matches_rename[i,]$stage %in% c("Round of 16", "Quarterfinals",
                                          "Semifinals", "Third place",
                                          "Final", "Final Round"))
  {
    matches_rename[i,]$stage = "Knockout"
  }
}

matches_rename %>% 
  count(stage)

# this is the edit from the errors before.
# Matches[704,]$winning_team = "Portugal"
# Matches[892,]$losing_team = "Colombia"

# this is the start of goals per stage, I am not sure I finished this
Home_Goals_by_stage = Matches %>% group_by(c(home_team, stage)) %>% 
  summarise(home_goals = sum(home_score), H_goals_against = sum(away_score),
            H_total_matches = n()) %>% 
  rename(country = home_team)

Away_Goals = Matches %>% group_by(away_team) %>% 
  summarise(away_goals = sum(away_score), A_goals_against = sum(home_score),
            A_total_matches = n()) %>% 
  rename(country = away_team)

# Calculation of records
wins = matches_rename %>% group_by(winning_team) %>% 
  filter(outcome != "D") %>% 
  summarize(total_wins = n()) %>% 
  rename(country = winning_team)

losses = matches_rename %>% group_by(losing_team) %>% 
  filter(outcome != "D") %>% 
  summarize(total_losses = n()) %>% 
  rename(country = losing_team)

draws_home = matches_rename %>% group_by(home_team) %>% 
  filter(outcome == "D") %>% 
  summarise(draws_home = n()) %>% 
  rename(country = home_team)

draws_away = matches_rename %>% group_by(away_team) %>% 
  filter(outcome == "D") %>% 
  summarise(draws_away = n()) %>% 
  rename(country = away_team)

Records = wins %>% 
  full_join(losses, by = "country")


Draws = full_join(draws_home, draws_away, by = "country")

# Remove NA
for(i in seq(1:length(Draws$country)))
{
  if(is.na(Draws[i,]$draws_home))
  {
    Draws[i,]$draws_home = 0
  }
  if(is.na(Draws[i,]$draws_away))
  {
    Draws[i,]$draws_away = 0
  }
}

Draws = Draws %>% 
  mutate(total_draws = draws_home + draws_away)

# Combines into one dataset
Records = wins %>% 
  full_join(losses, by = "country")

Records = wins %>% 
  full_join(losses, by = "country") %>% 
  full_join(Draws, by = "country") %>% 
  select(-c(draws_home, draws_away))

# remove NA
for(i in seq(1:length(Records$country)))
{
  for(j in seq(1:length(Records[i,])))
  {
    if(is.na(Records[i,j]))
    {
      Records[i,j] = 0
    }
  }
}

# mutating records to calculate win percentage
Records = Records %>% mutate(total_GP = total_wins + total_losses + total_draws) %>% 
  mutate(win_perc = (total_wins+total_draws*0.5)/total_GP)

wins_group = matches_rename %>% filter(stage == "Group") %>% 
  filter(outcome != "D") %>% 
  group_by(winning_team) %>% summarize(wins = n())

wins_knockout = matches_rename %>% filter(stage == "Knockout") %>% 
  filter(outcome != "D") %>% 
  group_by(winning_team) %>% summarize(wins = n())

losses = matches_rename %>% group_by(losing_team) %>% 
  summarize(h_wins = n())


# combine into one dataset
Countries_WC = Records %>% 
  full_join(Goals, by = "country")

for(i in seq(1:length(Countries_WC$country)))
{
  for(j in seq(1:length(Countries_WC[i,])))
  {
    if(is.na(Countries_WC[i,j]))
    {
      Countries_WC[i,j] = 0
    }
  }
}

Countries_WC = Countries_WC %>% mutate(total_goals = home_goals + away_goals) %>% 
  mutate(avg_diff = diff/total_matches)

# Calculate Correlations
cor(Countries_WC$total_goals, Countries_WC$total_wins, method = "spearman")
cor(Countries_WC$total_goals_against, Countries_WC$total_wins, method = "spearman")
cor(Countries_WC$total_goals, Countries_WC$total_losses, method = "spearman")
cor(Countries_WC$total_goals_against, Countries_WC$total_losses, method = "spearman")

cor(Countries_WC$goals_per_game, Countries_WC$total_wins, method = "spearman")
cor(Countries_WC$goals_against_per_game, Countries_WC$total_wins, method = "spearman")
cor(Countries_WC$goals_per_game, Countries_WC$total_losses, method = "spearman")
cor(Countries_WC$goals_against_per_game, Countries_WC$total_losses, method = "spearman")

cor(Countries_WC$goals_per_game, Countries_WC$win_perc, method = "spearman")
cor(Countries_WC$goals_against_per_game, Countries_WC$win_perc, method = "spearman")
cor(Countries_WC$diff, Countries_WC$total_wins, method = "spearman")
cor(Countries_WC$diff, Countries_WC$total_losses, method = "spearman")

cor(Countries_WC$diff, Countries_WC$win_perc, method = "spearman")
cor(Countries_WC$diff, Countries_WC$total_draws, method = "spearman")

write.csv(Countries_WC, "Countries_Stats.csv")
setdiff(Records$country, Goals$country)

Countries_WC %>% ggplot(aes(x = goals_against_per_game, y = win_perc,
                            label = country)) + geom_point() + 
  geom_text(hjust=0, vjust=-1, size = 2)
lm(Countries_WC$win_perc ~ Countries_WC$goals_against_per_game)
filter(Countries_WC, country == "Wales")

## This is the start of the linear regression work
row.number = sample(1:nrow(Countries_WC), 2/3*nrow(Countries_WC))
row.number

train_fifa = Countries_WC[row.number, ]
test_fifa = Countries_WC[-row.number, ]

fit1 = lm(win_perc ~ diff, data = train_fifa)
pred_vals_fifa = predict(fit1, newdata = test_fifa)
actual_vals_fifa = test_fifa$win_perc
rmse = sqrt(mean((actual_vals_fifa - pred_vals_fifa)^2))
rmse

Countries_WC_model = Countries_WC %>%
  dplyr::select(c(win_perc, goals_per_game, goals_against_per_game, diff))

train_fifa2 = Countries_WC_model[row.number, ]
test_fifa2 = Countries_WC_model[-row.number, ]

actual_vals_fifa2 = test_fifa2$win_perc
fit2 = lm(win_perc ~ ., data = train_fifa2)
pred_vals_fifa2 = predict(fit2, newdata = test_fifa2)
sqrt(mean((actual_vals_fifa - pred_vals_fifa2)^2))

