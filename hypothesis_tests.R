library(tidyverse)
library(liver)
World_cups = read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv")
Matches = read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/wcmatches.csv")

### Code Taken From Liam: ###

# filtering goals for, goals against, and total matches by country (when playing as the home team)
Home_Goals = Matches %>% group_by(home_team) %>% 
  summarise(home_goals = sum(home_score), H_goals_against = sum(away_score),
            H_total_matches = n()) %>% 
  rename(country = home_team)

# filtering foals for, goals against, and total matches by country (when playing as the away team)
Away_Goals = Matches %>% group_by(away_team) %>% 
  summarise(away_goals = sum(away_score), A_goals_against = sum(home_score),
            A_total_matches = n()) %>% 
  rename(country = away_team)

# Combining both home and away datasets into one
Goals = Home_Goals %>% 
  full_join(Away_Goals, by = "country")

# Remove NA values from data
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

### Liam's code ends here ###



### Kyle's code starts here ###

# Creating normalized data using the min-max method
# taking a subset of "Goals", with columns of interest
Goals_of_Interest = Goals[, c("country", "diff", "total_matches", "total_goals", "total_goals_against")]

# using the transform function from the liver library to standardize the data
Normalized_Goals = transform(Goals_of_Interest, method="zscore")

# the transform function also normalized the "country" column
# so we'll revert this change
Normalized_Goals[, "country"] = Goals[, "country"]

Normalized_Goals














