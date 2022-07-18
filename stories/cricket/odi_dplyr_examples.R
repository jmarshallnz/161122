library(cricketr)
library(tidyverse)
library(lubridate)

if (0) {
  odis <- getTeamData(matchType = "ODI")
  odis[names(odis) == ""] <- NULL
  
  write_csv(odis, here::here("stories/cricket/ODIs.csv"))
}

odis <- read_csv("stories/cricket/ODIs.csv") %>% extract(Score, into=c("Runs", "Wickets"), regex="([0-9]+)([/0-9])*", convert=TRUE, remove=FALSE) %>%
  mutate(Opposition = str_sub(Opposition, 3)) %>% replace_na(list(Wickets = 10)) %>%
  mutate(`Start Date` = dmy(`Start Date`)) %>%
  mutate(Year = year(`Start Date`)) %>%
  select(Team, Score=Runs, Wickets, Overs, Opposition, Result, Ground, Year)

odis %>% group_by(Ground) %>% mutate(GroundCount = n()) %>%
  group_by(Team) %>% mutate(TeamCount = n()) %>%
  filter(GroundCount > 50, TeamCount > 100) %>%
  ungroup() %>% select(-GroundCount, -TeamCount) -> odis_filtered

odis_filtered %>% write_csv("exam/ODIs.csv")

## Examples under here...

# OK, do some shit. Which team has the highest Score at <ground> ?Abu Dhabi?
odis %>% filter(Ground == "Abu Dhabi") %>%
  arrange(desc(Score)) # Pakistan

# What proportion of wins did <team> have in <year>?
odis %>% filter(Year == 2018, Team == "New Zealand") %>%
  count(Result)
