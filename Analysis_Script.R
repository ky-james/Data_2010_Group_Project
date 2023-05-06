### Liam's Code: FIFA Scoring System Analysis ###

World_cups = read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv")
Matches = read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/wcmatches.csv")

library(tidyverse)

## Setting Up Overall Statistics ##

# Get Total Goals #

Home_Goals = Matches %>% group_by(home_team) %>%
  group_by(year, .add = TRUE) %>% 
  summarise(home_goals = sum(home_score), H_goals_against = sum(away_score),
            H_total_matches = n()) %>% 
  rename(country = home_team)

Away_Goals = Matches %>% group_by(away_team) %>% 
  group_by(year, .add = TRUE) %>% 
  summarise(away_goals = sum(away_score), A_goals_against = sum(home_score),
            A_total_matches = n()) %>% 
  rename(country = away_team)

Goals = Home_Goals %>% 
  full_join(Away_Goals, by = c("country", "year"))

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

# Set Up Main Dataset
Goals_by_year = Goals %>% 
  mutate(total_goals = home_goals + away_goals,
         total_goals_against = H_goals_against + A_goals_against,
         diff = total_goals - total_goals_against,
         total_matches = H_total_matches + A_total_matches,
         goals_per_game = total_goals/total_matches,
         goals_against_per_game = total_goals_against/total_matches)

## Renaming Code ## (Not necessary for main score)
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

# Calculation of records by year
wins = matches_rename %>% group_by(winning_team) %>% 
  group_by(year, .add = TRUE) %>% 
  filter(outcome != "D") %>% 
  summarize(total_wins = n()) %>% 
  rename(country = winning_team)

losses = matches_rename %>% group_by(losing_team) %>%
  group_by(year, .add = TRUE) %>% 
  filter(outcome != "D") %>% 
  summarize(total_losses = n()) %>% 
  rename(country = losing_team)

draws_home = matches_rename %>% group_by(home_team) %>% 
  group_by(year, .add = TRUE) %>% 
  filter(outcome == "D") %>% 
  summarise(draws_home = n()) %>% 
  rename(country = home_team)

draws_away = matches_rename %>% group_by(away_team) %>% 
  group_by(year, .add = TRUE) %>% 
  filter(outcome == "D") %>% 
  summarise(draws_away = n()) %>% 
  rename(country = away_team)

Records = wins %>% 
  full_join(losses, by = c("country","year"))


Draws = full_join(draws_home, draws_away, by = c("country","year"))

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
  full_join(losses, by = c("country", "year"))

Records = wins %>% 
  full_join(losses, by = c("country", "year")) %>% 
  full_join(Draws, by = c("country", "year")) %>% 
  select(-c(draws_home, draws_away)) %>% 
  arrange(year) %>% arrange(country)

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

# Calculate Win Percentage
Records = Records %>% mutate(total_GP = total_wins + total_losses + total_draws) %>% 
  mutate(win_perc = (total_wins+total_draws*0.5)/total_GP)

# combine into one dataset
Countries_WC_Year = Records %>% 
  full_join(Goals_by_year, by = c("country", "year")) %>% 
  arrange(year) %>% arrange(country)

Countries_WC = Countries_WC_Year %>% 
  group_by(country) %>% 
  summarize(total_wins = sum(total_wins),
            total_losses = sum(total_losses),
            total_draws = sum(total_draws),
            total_GP = sum(total_GP),
            win_perc = (total_wins + total_draws*0.5)/total_GP,
            total_goals = sum(total_goals),
            total_goals_against = sum(total_goals_against),
            diff = sum(diff), goals_per_game = total_goals/total_GP,
            goals_against_per_game = total_goals_against/total_GP)

# Calculate Correlations (using totals (ie Countries_WC))
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

# Using Linear Regression to make model
set.seed(21)
row.number = sample(1:nrow(Countries_WC), 2/3*nrow(Countries_WC))

Countries_WC_model = Countries_WC %>%
  dplyr::select(c(win_perc, goals_per_game, goals_against_per_game))

train_fifa = Countries_WC_model[row.number, ]
test_fifa = Countries_WC_model[-row.number, ]

actual_vals_fifa = test_fifa$win_perc
fit = lm(win_perc ~ ., data = train_fifa)
pred_vals_fifa = predict(fit, newdata = test_fifa)
#RMSE
sqrt(mean((actual_vals_fifa - pred_vals_fifa)^2))

summary(fit)

## Normalization of Goals per World Cup
norm_goals = Countries_WC_Year %>% 
  group_by(year) %>% summarize(mean_gpg = mean(goals_per_game),
                               sd_gpg = sd(goals_per_game),
                               mean_gapg = mean(goals_against_per_game),
                               sd_gapg = sd(goals_against_per_game))

Countries_WC_Year = Countries_WC_Year %>% 
  left_join(norm_goals, by = "year")

# Normalizing using z-score since there is a lot of values
Norm_scores_fifa = Countries_WC_Year %>% 
  dplyr::select(c(country, year, goals_per_game, goals_against_per_game,
                  mean_gpg, sd_gpg, mean_gapg, sd_gapg)) %>% 
  mutate(norm_goals_per_game = (goals_per_game - mean_gpg)/sd_gpg,
         norm_goals_against_per_game = 
           (goals_against_per_game - mean_gapg)/sd_gapg,
         score = norm_goals_per_game - norm_goals_against_per_game) %>% 
  dplyr::select(c(country, year, score)) %>% arrange(desc(score))

head(Norm_scores_fifa, 10)


### Kyle's Code: Hypothesis Tests on German Teams ###
# PART 1: Germany Analysis
# Part A: Germany ANOVA test
east_germany_scores = subset(Norm_scores_fifa, country == "East Germany")$score
germany_scores = subset(Norm_scores_fifa, country == "Germany")$score
west_germany_scores = subset(Norm_scores_fifa, country == "West Germany")$score

germany_anova_data = data.frame(
  country = c(rep("East Germany", length(east_germany_scores)),
              rep("Germany", length(germany_scores)),
              rep("West Germany", length(west_germany_scores))),
  score = c(east_germany_scores, germany_scores, west_germany_scores))

germany_aov = aov(score ~ country, data = germany_anova_data)
summary(germany_aov)

# Part B: Germany two sample t-test 
split_germany_scores = c(east_germany_scores, west_germany_scores)
t.test(split_germany_scores, germany_scores, var.equal = TRUE)

# PART 2: Yugoslavia's Analysis
# Part A: Yugoslavia ANOVA test
yugoslavia_score = subset(Norm_scores_fifa, country %in% c("FR Yugoslavia", "Yugoslavia"))$score
croatia_score = subset(Norm_scores_fifa, country == "Croatia")$score
serbia_score = subset(Norm_scores_fifa, country == "Serbia")$score
slovenia_score = subset(Norm_scores_fifa, country == "Slovenia")$score

yugoslavia_anova_data = data.frame(
  country = c(rep("Yugoslavia", length(yugoslavia_score)),
              rep("Croatia", length(croatia_score)),
              rep("Serbia", length(serbia_score)),
              rep("Slovenia", length(slovenia_score))),
  score = c(yugoslavia_score, croatia_score, serbia_score, slovenia_score))

yugoslavia_aov = aov(score ~ country, data = yugoslavia_anova_data)
summary(yugoslavia_aov)

# Part B: Yugoslavia two sample t-test
split_yugoslavia_scores = c(croatia_score, serbia_score, slovenia_score)
t.test(split_yugoslavia_scores, yugoslavia_score, var.equal = TRUE)








