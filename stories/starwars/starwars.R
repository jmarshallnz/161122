library(tidyverse)

data("starwars", package='dplyr')

films <- starwars %>% unnest(films)

people <- c("Luke Skywalker", "R2-D2", "Yoda", "Leia Organa", "Rey")

films %>% filter(name %in% people) %>% as.data.frame

characters <- starwars %>% filter(name %in% people) %>% select(name, eye_color, gender, species)

films <- starwars %>% unnest(films) %>% select(film=films, name) %>% arrange(film) %>%
  filter(film %in% c("A New Hope", "The Empire Strikes Back", "Return of the Jedi"),
         name %in% c(people, "C-3PO"))
write_csv(characters, here::here("data/starwars/sw_characters.csv"))
write_csv(films, here::here("data/starwars/sw_films.csv"))

characters %>% left_join(films)
characters %>% right_join(films)
characters %>% inner_join(films)
characters %>% full_join(films)
