library(tidyverse)
library(tidytext)

threewords <- read_csv('stories/three_words/three_words.csv')

threewords

word_list <- threewords |>
  pivot_longer(-Timestamp,
               names_to="Which",
               values_to="Word")
word_list

word_list |>
  mutate(word = str_to_lower(Word)) |>
  unnest_tokens(word, word) |>
  anti_join(stop_words) |>
  count(word) |>
  filter(n > 1) |>
  ggplot() +
  geom_col(mapping=aes(y=fct_reorder(word, n), x=n))




ggplot(data=word_list) +
  geom_bar(mapping=aes(y=Word))

word_list |>
  mutate(word = str_to_lower(Word)) |>
  unnest_tokens(word, word) |>
  anti_join(stop_words) |>
  count(word) |>
  filter(n > 1) |>
  ggplot() +
  geom_col(mapping=aes(y=fct_reorder(word, n), x=n))


words <- word_list |>
  mutate(word = str_to_lower(Word)) |>
  unnest_tokens(word, word) |>
  anti_join(stop_words) |>
  count(word)

words |>
  arrange(desc(n)) |>
  print(n=30)

words |>
  filter(n > 1) |>
  ggplot() +
  geom_col(aes(y=fct_reorder(word, n, identity), x=n)) +
  labs(y=NULL, x="Number of students")






three_words <- read_csv("stories/three_words/What do you think statistics is (Responses) - Form Responses 1.csv")
three_words %>%
  mutate(Timestamp = lubridate::mdy_hms(Timestamp)) %>%
  filter(Timestamp > '2022-01-01') %>%
  write_csv("stories/three_words/three_words.csv")

shortened <- words %>% select(word) %>% filter(!str_ends(word, "s")) %>%
  mutate(short = word, word = paste0(word, 's')) %>%
  unique()

words %>% left_join(shortened) %>%
  mutate(word = ifelse(!is.na(short), short, word)) %>%
  count(word) -> words

mutate(stem_word = hunspell_stem(word, dict=dictionary("en_AU")))

dist <- as.matrix(stringdistmatrix(words$word, method="lv"))
rownames(dist) <- words$word
colnames(dist) <- words$word
plot(hclust(as.dist(dist)))

mutate(check = hunspell_check(word, dict = dictionary("en_AU")))
mutate(stem_word = hunspell_stem(word, dist=dictionary("en_AU"))) %>%
  unnest(stem_word)

group_by(word) %>% summarise(n = first(n), stem = stem_word[which.min(nchar(stem_word))]) %>%
  mutate(stem = ifelse(stem == "ma", "mathematics", stem),
         stem = ifelse(stem == "graphs", "graph", stem))

count(word) %>%
  as.data.frame()
