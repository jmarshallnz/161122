library(tidyverse)

cheats <- read_csv("stories/ncea_cheats/cheats.csv")

roll <- read_csv("https://www.massey.ac.nz/~jcmarsha/data/schools/roll_nomacrons.csv")
schools <- read_csv("https://www.massey.ac.nz/~jcmarsha/data/schools/schools.csv")

school_roll <- roll %>% extract(Level, into='Level', regex="Year (.*)", convert=TRUE) %>%
  filter(Level >= 11) %>%
  group_by(School) %>%
  summarise(Students = sum(Students)) %>%
  left_join(schools) %>%
  group_by(RegionalCouncil) %>%
  summarise(Students = sum(Students)) %>%
  rename(Region = RegionalCouncil) %>%
  mutate(Region = fct_collapse(Region,
                               'Nelson/Marlborough' = c('Nelson', 'Marlborough'),
                               `Hawke's Bay` = 'Hawkes Bay')) %>%
  group_by(Region) %>%
  summarise(Students = sum(Students))

cheats %>% mutate(Region = fct_collapse(Region,
                                      Gisborne = 'East Coast',
                                      `Manawatu-Whanganui` = c('Manawatu', "Wanganui"),
                                      Wellington = c('Wellington', 'Wairarapa'))) %>%
  filter(Region != "Central Plateau") %>%
  group_by(Region) %>%
  summarise(Cheats = sum(Cheats)) %>%
  left_join(school_roll) %>%
  write_csv("stories/ncea_cheats/ncea_cheats.csv")

yearly <- tibble::tribble(~Year, ~Cheats,
                          2011, 299,
                          2012, 354,
                          2013, 273,
                          2014, 280,
                          2015, 290)
yearly %>% write_csv("stories/ncea_cheats/ncea_cheats_yearly.csv")
